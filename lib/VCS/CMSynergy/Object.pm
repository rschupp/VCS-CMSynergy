package VCS::CMSynergy::Object;

# Copyright (c) 2001-2010 argumentum GmbH, 
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

use strict;
use warnings;

=head1 NAME

VCS::CMSynergy::Object - convenience wrapper to treat objectnames as an object

=head1 SYNOPSIS

  use VCS::CMSynergy;
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  $obj = $ccm->object($name, $version, $cvtype, $instance);
  $obj = $ccm->object($objectname);
  print ref $obj;			# "VCS::CMSynergy::Object"

  # objectname and its constituents
  print "...and the object is $obj";
  print "name       = ", $obj->name;
  print "version    = ", $obj->version;
  print "cvtype     = ", $obj->cvtype;
  print "instance   = ", $obj->instance;
  print "objectname = ", $obj->objectname;

  # attribute methods, optionally caching with 
  #   use VCS::CMSynergy ':cached_attributes'
  print $obj->get_attribute('comment');
  $obj->set_attribute(comment => "blurfl");
  $obj->create_attribute("foo", string => "some text");
  $obj->delete_attribute("foo");
  $hashref = $obj->list_attributes;	# always caches result

  # property methods
  print $obj->property("bar");
  print $obj->displayname;		# always caches result

  ## tiehash interface
  use VCS::CMSynergy ':tied_objects';
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  print $obj->{comment};	
  $obj->{comment} = "blurfl";	
  # same as:
  #   print $ccm->get_attribute(comment => $obj);
  #   $ccm->set_attribute(comment => $obj, "blurfl");


This synopsis only lists the major methods.

=cut 

use base qw(Class::Accessor::Fast);
__PACKAGE__->mk_ro_accessors(qw/objectname ccm/);

use Carp;

use Type::Params qw( validate );
use Types::Standard qw( slurpy Str ArrayRef );

# NOTE: We can't just alias string conversion to objectname()
# as it is called (as overloaded operator) with three arguments
# which Class::Accessor's ro accessors dont't like.
use overload 
    '""'	=> sub { $_[0]->objectname },
    cmp		=> sub { $_[0]->objectname cmp $_[1]->objectname },
    fallback	=> 1;

my $have_weaken = eval "use Scalar::Util qw(weaken); 1";


my %cvtype2subclass = 
( 
    project	=> "Project",
);


# VCS::CMSynergy::Object->new(ccm, objectname)
# factory method
sub new
{
    unless (@_ == 3)
    {
	carp(__PACKAGE__ . "::new: illegal number of arguments");
	return;
    }
    my ($class, $ccm, $objectname) = @_;

    return $ccm->set_error("invalid objectname `$objectname'")
	unless $objectname =~ /$ccm->{objectname_rx}/;

    # canonicalize delimiter to colon
    $objectname =~ s/$ccm->{delimiter_rx}/:/;

    # return "unique" object if we already "know" it
    return $ccm->{objects}->{$objectname} if $ccm->{objects}->{$objectname};

    my %fields = (
        objectname => $objectname,
        ccm        => $ccm,
    );
    Scalar::Util::weaken($fields{ccm}) if $have_weaken;
    $fields{acache} = {} if VCS::CMSynergy::use_cached_attributes();

    if (my $subclass = $cvtype2subclass{ (split(":", $objectname))[2] })
    {
	require "VCS/CMSynergy/$subclass.pm";
	$class = "VCS::CMSynergy::$subclass";
    }

    my $self;
    if (VCS::CMSynergy::use_tied_objects())
    {
	$self = bless {}, $class;
	tie %$self, 'VCS::CMSynergy::ObjectTieHash', \%fields;
    }
    else
    {
	$self = bless \%fields, $class;
    }

    # remember new object 
    $ccm->{objects}->{$objectname} = $self;

    return $self;
}

# access to the parts of the objectname
# NOTE: DON'T use "shift->{objectname}" as it won't work for :tied_objects
sub name        { return (split(":", shift->objectname))[0] }
sub version     { return (split(":", shift->objectname))[1] }
sub cvtype      { return (split(":", shift->objectname))[2] }
sub instance    { return (split(":", shift->objectname))[3] }

# convenience methods for frequently used tests
sub is_dir	{ return shift->cvtype eq "dir"; }
sub is_project	{ return shift->cvtype eq "project"; }


# NOTE: All access to a VCS::CMSynergy::Objects data _must_ either use
# methods, e.g. "$self->foo", or use _private(), e.g. "$self->_private->{foo}".
# _Don't_ access its member directly, e.g. "$self->{foo}", because this
# doesn't work when :tied_objects are enabled.
# The only exception to this rule are the primary getter methods (objectname,
# version etc) which use direct access for speed. Hence they need to be
# redefined in ObjectTieHash.pm.

# access to private parts
sub _private 	{ return shift; }

sub mydata
{
    my $self = shift;
    return $self->_private->{mydata} ||= {};
}


sub list_attributes
{
    my $self = shift;
    return $self->ccm->list_attributes($self);
}

sub get_attribute
{
    my $self = shift;
    my ($name) = validate(\@_, Str);

    if (VCS::CMSynergy::use_cached_attributes())
    {
	my $acache = $self->_private->{acache};
	return $acache->{$name} if exists $acache->{$name};
    }

    my $value = $self->ccm->get_attribute($name, $self);

    $self->_update_acache($name => $value);
    return $value;
}

sub set_attribute
{
    my $self = shift;
    my ($name, $value) = validate(\@_, Str, Str);

    my $rc = $self->ccm->set_attribute($name, $self, $value);

    if (defined $rc) { $self->_update_acache($name => $value); }
    else             { $self->_forget_acache($name); }

    return $rc;
}

sub create_attribute
{
    my $self = shift;
    my ($name, $type, $value) = validate(\@_, Str, Str, Str);

    my $rc = $self->ccm->create_attribute($name, $type, $value, $self);

    # update attribute cache if necessary
    $self->_update_acache($name => $value) if $rc;

    return $rc;
}

sub delete_attribute
{
    my $self = shift;
    my ($name) = validate(\@_, Str);

    my $rc = $self->ccm->delete_attribute($name, $self);

    # update attribute cache if necessary
    # NOTE: the attribute may have reverted from local back to inherited
    $self->_forget_acache($name) if $rc; 	

    return $rc;
}

sub copy_attribute
{
    my $self = shift;
    my ($names, $to_file_specs) = 
        validate(\@_, Str | ArrayRef[Str], slurpy ArrayRef);
    $names = [ $names ] unless ref $names;

    # NOTE: no $flags allowed, because honouring them would need
    # a project traversal to update or invalidate attribute caches

    my $rc = $self->ccm->copy_attribute($names, [], $self, @$to_file_specs);

    if (VCS::CMSynergy::use_cached_attributes())
    {
	my @objects = grep { UNIVERSAL::isa($_, 'VCS::CMSynergy') } @$to_file_specs;
	my $acache = $self->_private->{acache};

	foreach my $name (@$names)
	{
	    if ($rc && exists $acache->{$name})
	    {
		# if we already know the value of the copied attribute(s)
		# and the copy was successful, update the targets' caches
		my $value = $acache->{$name};
		$_->_update_acache($name => $value) foreach @objects;
	    }
	    else
	    {
		# in all other cases, invalidate the targets' caches
		# (esp. in case of failure, since we can't know 
		# which got actually updated)
		$_->_forget_acache($name) foreach @objects;
	    }
	}
    }

    return $rc;
}

# $obj->_update_acache($name => $value) or
# $obj->_update_acache(\%attributes)
sub _update_acache
{
    return unless VCS::CMSynergy::use_cached_attributes();

    my $self = shift;
    if (@_ == 2)
    {
	$self->_private->{acache}{$_[0]} = $_[1];
    }
    else
    {
	my $attrs = shift;
	@{$self->_private->{acache}}{keys %$attrs} = values %$attrs;
    }
}

# $obj->_forget_acache(@names)
sub _forget_acache
{
    return unless VCS::CMSynergy::use_cached_attributes();

    my $self = shift;
    delete $self->_private->{acache}{$_} foreach @_;
}


# test whether object exists (without causing an exception)
sub exists
{
    my $self = shift;
    my ($rc) = $self->ccm->_ccm(qw/attribute -show version/, $self);
    return $rc == 0;
}

sub property
{
    my $self = shift;
    my ($keyword_s) = validate(\@_, Str | ArrayRef[Str]);

    my $props = $self->ccm->property($keyword_s, $self);
    $self->_update_acache(ref $keyword_s ? $props : { $keyword_s => $props });
    return $props;
}

sub displayname
{
    my $self = shift;
    # cache this property (because it's immutable)
    # in the attribute cache (even if not using :cached_attributes);
    # this will do the right thing wrt caching, e.g.
    #
    #    my $result = $ccm->query_object("...", qw( ... displayname ... ));
    #    foreach (@$result) {
    #      ... $_->displayname ...      # cached, no "ccm property ..." called
    #    }
    return $self->_private->{acache}{displayname} ||= $self->property('displayname');
}

sub cvid
{
    my $self = shift;
    # cache this property (because it's immutable)
    # in the attribute cache (even if not using :cached_attributes);
    # this will do the right thing wrt caching, e.g.
    #
    #    my $result = $ccm->query_object("...", qw( ... cvid ... ));
    #    foreach (@$result) {
    #      ... $_->cvid ...             # cached, no "ccm property ..." called
    #    }
    return $self->_private->{acache}{cvid} ||= $self->property('cvid');
}

sub cat_object
{
    my $self = shift;
    # NOTE: careful here to correctly handle the case when 
    # no destination was given
    return $self->ccm->cat_object($self, @_);
}

# $obj->is_foo_of: short for $ccm->query_object({is_foo_of => [ $obj ]})
# same for has_foo
sub AUTOLOAD
{
    my $this = shift;

    our $AUTOLOAD;

    # NOTE: the fully qualified name of the method has been placed in $AUTOLOAD
    my ($class, $method) = $AUTOLOAD =~ /^(.*)::([^:]*)$/;
    return if $method eq 'DESTROY'; 

    # we don't allow autoload of class methods
    croak("Can't locate class method \"$method\" via class \"$class\"")
	unless ref $this;

    if ($method =~ /^(is_.*_of|has_.*)$/)
    {
	return $this->ccm->query_object("$method('$this')", @_);
    }
    croak("Can't locate object method \"$method\" via class \"$class\"");
}


1;

__END__

=head1 DESCRIPTION

A C<VCS::CMSynergy::Object> is mostly a glorified wrapper for
a CM Synergy's objectname (sometimes called I<object reference form>
in CM Synergy documentation). Because of its overloaded string conversion
method (see below), it can be used with C<VCS::CMSynergy> methods wherever
an objectname would be appropriate, esp. where the documentation
specifies a B<file_spec>.

When L<VCS::CMSynergy::Object/:cached_attributes> is in effect,
a C<VCS::CMSynergy::Object> keeps a "demand loaded"
cache of attribute names and values.

There is also a L</TIEHASH INTERFACE> for 
manipulating an object's attributes using the hash notation.

=head1 BASIC METHODS

=head2 new

  # let $ccm be a VCS::CMSynergy
  $obj = VCS::CMSynergy::Object->new($ccm, $objectname);

  # more conveniently
  $obj = $ccm->object($name, $version, $cvtype, $instance);
  $obj2 = $ccm->object("name~version:cvtype:instance");

Create a C<VCS::CMSynergy::Object> from a Synergy session and
an objectname (sometimes called I<object reference form> in Synergy documentation).
You may use either the database delimiter or a colon to separate the name
and version parts of the objectname.

Usually you would not call this method directly, but rather
via the wrapper L<VCS::CMSynergy/object>.

Note that no check is made whether the corresponding object really exists
in the Synergy database, use L</exists> for that.

Invoking C<new> several times with the same objectname
always returns the I<same> C<VCS::CMSynergy::Object>. This also holds
for any method that returns C<VCS::CMSynergy::Object>s
(by calling C<new> implicitly), e.g. L<VCS::CMSynergy/object> or
L<VCS::CMSynergy/query_object>.

=head2 objectname 

  print $obj->objectname;

Returns the object's complete name in I<object reference form>,
i.e. C<"name:version:cvtype:instance">. 
Note that the delimiter between name and version is canonicalized to a colon,
i.e. independent of the value of database delimiter.

=head2 name, version, cvtype, instance

  print $obj->name;
  print $obj->version;
  print $obj->cvtype;
  print $obj->instance;

Returns the object's I<name>, I<version>, I<type>, or I<instance>, resp.

=head2 string conversion

C<VCS::CMSynergy::Object> overloads string conversion with
L</objectname>, i.e. the following expressions evaluate to the same string:

  "$obj"  
  $obj->objectname

This makes it possible to use a C<VCS::CMSynergy::Object> throughout
C<VCS::CMSynergy> wherever an objectname would have been appropriate.

=head2 is_project, is_dir

  if ($obj->is_project) { ... }

These are convenience functions that test whether the object's I<type>
is C<"project"> or C<"dir">, resp.

=head2 ccm

  $obj->ccm->query_hashref(...);

C<ccm> returns the session (a C<VCS::CMSynergy>) that is associated
with the object.

=head2 cat_object

  $contents = $obj->cat_object();
  $obj->cat_object($destination);

A convenience wrapper for L<VCS::CMSynergy/cat_object>.

=head2 mydata

Sometimes it is handy to be able to store some arbitrary data 
into a C<VCS::CMSynergy::Object>. This method returns a reference
to a hash associated with the object. It is totally opaque
w.r.t. Synergy operations. 

=head1 ATTRIBUTE METHODS

=head2 get_attribute, set_attribute

  $value = $obj->get_attribute($attribute_name);
  $obj->set_attribute($attribute_name, $value);

These are convenience wrappers for L<VCS::CMSynergy/get_attribute>
and L<VCS::CMSynergy/set_attribute>, resp., i.e.

  print $obj->get_attribute("comment");

is syntactic sugar for

  print $ccm->get_attribute("comment", $obj);

If you are C<use>ing L<VCS::CMSynergy/:cached_attributes>, 
these methods maintain a cache
of attribute names and values in the object. Note that this cache
is only consulted if you use C<VCS::CMSynergy::Object> methods
(including the L</TIEHASH INTERFACE>) and will get inconsistent if you
mix C<VCS::CMSynergy::Object> and C<VCS::CMSynergy> calls
on the same object.

=head2 create_attribute, delete_attribute

  $obj->create_attribute($attribute_name, $attribute_type, $value);
  $obj->delete_attribute($attribute_name);

Convenience wrappers for L<VCS::CMSynergy/create_attribute> and  
L<VCS::CMSynergy/delete_attribute>, resp. Also update the cache
when L<VCS::CMSynergy/:cached_attributes> is in effect.

=head2 copy_attribute

  $obj->copy_attribute($attribute_name, @to_file_specs);

Convenience wrapper for L<VCS::CMSynergy/copy_attribute>.
Also invalidate the cache entries for C<$attribute_name> for all
C<VCS::CMSynergy::Object>s in C<@to_file_specs>
when L<VCS::CMSynergy/:cached_attributes> is in effect.

Note: The optional C<$flags> parameter of L<VCS::CMSynergy/copy_attribute> is
not supported, because it would mean traversing the target projects
to update or invalidate attribute caches.

=head2 list_attributes

  $hashref = $obj->list_attributes;

Convenience wrapper for L<VCS::CMSynergy/list_attributes>. 

Note that the returned hash is always cached in the object
(and updated for successful L</create_attribute> and 
L</delete_attribute> calls).

=head2 exists

  print "$obj doesn't exist" unless $obj->exists;

Tests whether the C<VCS::CMSynergy::Object> corresponds to an object
in the CM Synergy database (without causing an exception if it doesn't).

=head1 PROPERTY METHODS

=head2 property

  $value = $obj->property($keyword);
  $hash = $obj->property(\@keywords);

Convenience wrapper for L<VCS::CMSynergy/property>, equivalent to

  $value = $ccm->property($keyword, $obj);
  $hash = $ccm->property(\@keywords, $obj);

=head2 displayname, cvid

  print $obj->displayname;
  print $obj->cvid;

Short hand for C<< $obj->property("displayname") >> or
C<< $obj->property("cvid") >>, resp. However, these two methods
cache their return value in the C<VCS::CMSynergy::Object>
(because it is immutable). 
If L<VCS::CMSynergy/:cached_attributes> is in effect, the cache
may be primed using L<VCS::CMSynergy/query_object> or similar methods, e.g.

  $result = $ccm->query_object("...", qw( ... displayname ... ));

=head1 is_RELATION_of, has_RELATION

  $tasks = $obj->has_associated_cv;

These are convenience methods to quickly enumerate all objects that
are somehow related to the invoking object:

  $obj->is_RELATION_of
  $obj->has_RELATION

are exactly the same as

  $obj->ccm->query_object("is_RELATION_of('$obj')")
  $obj->ccm->query_object("has_RELATION('$obj')")

If you supply extra arguments then these are passed down
to L<VCS::CMSynergy/query_object> as additional keywords.

See the CM Synergy documentation for the built-in relations. Note that it's
not considered an error to use a non-existing relation, the methods
will simply return (a reference to) an empty list.
This is consistent with the behaviour of B<ccm query> in this case.

=head1 TIEHASH INTERFACE

  use VCS::CMSynergy ':tied_objects';
  ...
  print $obj->{comment};	
  $obj->{comment} = "blurfl";	

When C<use>ing L<VCS::CMSynergy/:tied_objects>,
you can use a C<VCS::CMSynergy::Object> in the same way you
would use a hash reference. The available keys are the underlying
CM Synergy object's attributes. 

Note that contrary to the behaviour of real hashes, keys don't
spring into existence "on demand". Getting or setting the value
of an attribute that does not exist for the underlying CM Synergy object
will return C<undef> or throw an excpetion (depending on your sessions's
setting of L<VCS::CMSynergy/RaiseError>). 
However, testing for the existence of an attribute
with C<exists> works as expected.

NOTE: When using L<VCS::CMSynergy/:tied_objects>, it is strongly recommended
to have L<Scalar::Util|"the Scalar::Util module"> 
installed.  See L<Why is Scalar::Util recommended?> for an explanation.

=head2 FETCH, STORE

  $value = $obj->{attribute_name};
  $obj->{attribute_name} = $value;

These are wrappers for L</get_attribute> and L</set_attribute>, resp.
The operate on the same cache as these when
using  L<VCS::CMSynergy/:cached_attributes>

=head2 EXISTS

Checks the return value from L</list_attributes> for the existence
of the key (attribute) given.

=head2 FIRSTKEY, NEXTKEY

  foreach (@{ $obj->keys })  { ... }
  foreach (@{ $obj->values })  { ... }
  while (my ($attr, $val) = each %$obj)  { ... }

These methods use L</list_attributes> to obtain a list of attributes and then
iterate over this list. Hence C<keys>, C<values>, and C<each>
all work as expected. 

Warning: Enumerating the keys (i.e. attribute names) of a
tied  C<VCS::CMSynergy::Object>
is cheap (at most one call to B<ccm attribute -la>), but enumerating
the values may result in lots of calls to B<ccm attribute -show>.
Tools like L<Data::Dumper> or similar will implicitly enumerate all keys and
values when invoked on a tied object. This is especially annoying
when using the graphical Perl debugger L<Devel::ptkdb> and mousing
over a variable holding a tied object, because the debugger
uses  L<Data::Dumper> to construct a printable representation of
the object.

=head2 Why is Scalar::Util recommended?

Every C<VCS::CMSynergy::Object> keeps a reference to the session
(a C<VCS::CMSynergy>) where it was created in. It needs this "back pointer"
so it can implement methods that invoke CM Synergy operations,
e.g. L</get_attribute>. These references can
prevent the timely garbage collection of a session (esp. 
B<ccm stop> called from L<VCS::CMSynergy/DESTROY>) if a  
C<VCS::CMSynergy::Object> has a longer lifetime than its session.
(The latter is actually a programmer error, but there's no way
to enforce the correct lifetime rule.)
To work around this we need to make the session reference
not count w.r.t. to garbage collection. We use L<Scalar::Util/weaken> 
for that. If the C<VCS::CMSynergy> object goes away B<before> 
the C<VCS::CMSynergy::Object> gets destroyed, its session reference
will become C<undef>. 
Any method called on the C<VCS::CMSynergy::Object> after this point
that tries to invoke a session method 
will result in a (rightly deserved) error 
(C<Can't call method "..." on an undefined value>).

=head1 SEE ALSO

L<VCS::CMSynergy>
