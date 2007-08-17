package VCS::CMSynergy::Object;

our $VERSION = sprintf("%d.%02d", q%version: 1.14 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy::Object - Convenience wrapper to treat objectnames as an object

=head1 SYNOPSIS

  use VCS::CMSynergy;

  $ccm = VCS::CMSynergy->new(%attr);

  $obj1 = $ccm->object($name, $version, $cvtype, $instance);
  $obj2 = $ccm->object($objectname);

  print "...and the object is $obj";
  print "name     =", $obj->name;
  print "version  =", $obj->version;
  print "cvtype   =", $obj->cvtype;
  print "instance =", $obj->instance;
  print $obj1->objectname;

  ## tiehash interface

  print $obj->{comment};	
  # same as print $ccm->get_attribute(comment => $obj)

  $obj->{comment} = "blurfl";	
  # same as $ccm->set_attribute(comment => $obj, "blurfl")

  $obj->create_attribute("foo", "text");
  $obj->delete_attribute("foo");
  $hashref = $obj->list_attributes;
  print $obj->displayname;

This synopsis only lists the major methods.

=cut 

use Carp;

use overload 
    '""'	=> \&objectname,
    cmp		=> sub { $_[0]->objectname cmp $_[1]->objectname },
    fallback	=> 1;

# NOTE: It's not sufficient to simply require Scalar::Util,
# it must support weaken, too.
my $use_tiehash = eval { require Scalar::Util; import Scalar::Util qw(weaken); 1 };
require VCS::CMSynergy::ObjectTieHash if $use_tiehash;

# VCS::CMSynergy::Object->new(ccm, name, version, cvtype, instance)
sub new
{
    unless (@_ == 6)
    {
	carp(__PACKAGE__ . " new: illegal number of arguments");
	return undef;
    }
    my $class = shift;
    my $self = {};

    if ($use_tiehash)
    {
	tie %$self, 'VCS::CMSynergy::ObjectTieHash', @_;
    }
    else
    {
	my $delim = shift->delimiter;
	$self->{objectname} = "$_[0]${delim}$_[1]:$_[2]:$_[3]";
	@$self{qw(name version cvtype instance)} = @_;
    }

    return bless $self, $class;
}

{
    no strict 'refs';

    # generate getter methods
    foreach my $method (qw(objectname name version cvtype instance))
    {
	*{$method} = $use_tiehash ?
	    sub { my $self = tied %{$_[0]}; $self->{$method}; } :
	    sub { $_[0]->{$method} };
    }

    if ($use_tiehash)
    {
	# generate redirect-to-tied-self methods
	foreach my $method (
	    qw(create_attribute delete_attribute copy_attribute list_attributes
	       property displayname))
	{
	    *{$method} = sub { my $self = shift; (tied %$self)->$method(@_); };
	}
    }
}

sub proj_vers	
{ 
    my $self = shift;
    $self = tied %$self if $use_tiehash;

    carp(__PACKAGE__ . " proj_vers: not a project: $self") unless $self->cvtype eq 'project';

    (my $proj_vers = $self->objectname) =~ s/:.*//;
    return $proj_vers;
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

There is also a L</TIEHASH INTERFACE> for 
manipulating an object's attributes using the hash notation.

=head1 METHODS

=head2 new

Create a C<VCS::CMSynergy::Object> from a CM Synergy session and
either an objectname
(sometimes called I<object reference form> in CM Synergy documentation)
in "name-version:cvtype:instance" format or the four parts specified
separately. 

Usually you would not call this method directly, but rather
via the wrapper L<VCS::CMSynergy/object>.

Note that no check is made whether the specified object really exists
in the database.

=head2 objectname 

Returns the object's complete name in I<object reference form>.

=head2 name 

Returns the object's I<name>.

=head2 version 

Returns the object's I<version>.

=head2 cvtype 

Returns the object's I<type>.

=head2 instance 

Returns the object's I<instance> (also called I<subsystem> 
in older documentation).

=head2 string conversion

C<VCS::CMSynergy::Object> overloads string conversion with
L</objectname>, i.e. the following expressions evaluate to the same string:

  "$obj"  
  $obj->objectname

This makes it possible to use a C<VCS::CMSynergy::Object> throughout
C<VCS::CMSynergy> wherever an objectname would have been appropriate.

=head2 proj_vers

Returns 

  $obj->name . "_" . $obj->version

(i.e. the I<displayname> for a project object), but also checks that  
C<< $obj->cvtype >> actually is "project".

=head1 TIEHASH INTERFACE

  print $obj->{comment};	
  $obj->{comment} = "blurfl";	

NOTE: The tiehash interface and all other methods described below
are only available when you have L<Scalar::Util|"the Scalar::Util module"> 
installed.  See L<Why is Scalar::Util needed?> for an explanation.

You can use a C<VCS::CMSynergy::Object> reference in the same way you
would use a hash reference. The available keys are the underlying
CM Synergy object's attributes. 

Note that contrary to the behaviour of real hashes, keys don't
spring into existence "on demand". Getting or setting the value
of an attribute that does not exist for the underlying CM Synergy object
will return C<undef> or throw an excpetion (depending on your sessions's
L<VCS::CMSynergy/RaiseError> option). 
However, C<exists> works as expected.

=head2 FETCH

  $value = $obj->{attribute_name};

A wrapper for L<VCS::CMSynergy/get_attribute>. 

=head2 STORE

  $obj->{attribute_name} = $value;

A wrapper for L<VCS::CMSynergy/set_attribute>. 

=head2 EXISTS

Checks the return value from L</list_attributes> for the existence
of the key (attribute) given.

=head2 FIRSTKEY, NEXTKEY

  foreach (@{ $obj->keys })  { ... }
  foreach (@{ $obj->values })  { ... }
  while (my ($attr, $val) = each %$obj)  { ... }

These use L</list_attributes> to obtain a list of attributes and then
iterate over this list. Hence C<keys>, C<values>, and C<each>
all work as expected.

=head2 create_attribute

  $obj->create_attribute($attribute_name, $attribute_type);

A wrapper for L<VCS::CMSynergy/create_attribute>. 

=head2 delete_attribute

  $obj->delete_attribute($attribute_name");

A wrapper for L<VCS::CMSynergy/delete_attribute>. 

=head2 copy_attribute

  $obj->copy_attribute($attribute_name, $flags, @to_file_specs);

A wrapper for L<VCS::CMSynergy/copy_attribute>. 

=head2 list_attributes

  $hashref = $obj->list_attributes;

A wrapper for L<VCS::CMSynergy/list_attributes>. 

Note that returned hash is cached in the C<VCS::CMSynergy::Object>
(and updated for successfull L</create_attribute> and L</delete_attribute>).

=head2 property

  $value = $obj->property($keyword);

A wrapper for L<VCS::CMSynergy/property>. 

=head2 displayname

  print $obj->displayname;

Short hand for C<< $obj->property("displayname") >>, except that the return
value is cached in the C<VCS::CMSynergy::Object>.

=head2 Why is Scalar::Util needed?

In order to implement methods that actually invoke CM Synergy operations,
every C<VCS::CMSynergy::Object> has to keep a reference to the session
(a C<VCS::CMSynergy>) wherein it was created. These references can
stall the timely garbage collection of a session (including the
B<ccm stop> called from L<VCS::CMSynergy/DESTROY>) if a  
C<VCS::CMSynergy::Object> has a longer lifetime than its session.
To prevent this we need to make the reference to the session
not count w.r.t. to garbage collection. We use L<Scalar::Util/weaken> 
for that. If the session goes away before the C<VCS::CMSynergy::Object>
gets destroyed, its session reference will become C<undef>. Hence
any method called on the C<VCS::CMSynergy::Object> which involves a 
CM Synergy operation will result in an error 
C<Can't call method "..." on an undefined value>.

=head1 SEE ALSO

L<VCS::CMSynergy>
