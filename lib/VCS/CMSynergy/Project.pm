package VCS::CMSynergy::Project;

# Copyright (c) 2001-2015 argumentum GmbH
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

use strict;
use warnings;

=head1 NAME

VCS::CMSynergy::Project - convenience methods for C<VCS::CMSynergy::Object>s of type C<"project">

=head1 SYNOPSIS

C<VCS::CMSynergy::Project> is a subclass of L<VCS::CMSynergy::Object>
with additional methods for Synergy projects.

  use VCS::CMSynergy;
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  $proj = $ccm->object("editor-1:project:1");
  print ref $proj;			# "VCS::CMSynergy::Project"

  $proj->chdir_into_wa;

  $proj->traverse(
    sub { print "  " x VCS::CMSynergy::Traversal::depth(), $_, "\n"; } );

This synopsis only lists the major methods.

=cut 

use base qw(VCS::CMSynergy::Object);

use Carp;
use Type::Params qw( validate );
use Types::Standard qw( Str Optional InstanceOf Maybe
                        ArrayRef CodeRef HashRef );
use File::Spec;
use Cwd;

use VCS::CMSynergy::Client qw(_KEYWORDS);


=head1 WORKAREA METHODS

=head2 chdir_into_wa

  my $old_pwd = $proj->chdir_into_wa;

Changes into the toplevel workarea directory of project C<$proj>.
Returns C<undef> if C<$proj> doesn't maintain a workarea or 
the C<chdir()> failed, otherwise returns the name of current working
directory before the call.

=cut

# FIXME needs test
sub chdir_into_wa
{
    my $self = shift;
    return $self->ccm->set_error("project `$self' doesn't maintain a workarea")
	unless $self->get_attribute("maintain_wa") eq "TRUE";

    my $wa_top = File::Spec->catfile($self->get_attribute("wa_path"), $self->name);
    my $old_pwd = cwd();
    chdir($wa_top) 
	or return $self->ccm->set_error("can't chdir($wa_top) into workarea of project `$self': $!");
    return $old_pwd;
}


=head1 PROJECT TRAVERSAL

=head2 traverse

  $proj->traverse(\&wanted, $dir);
  $proj->traverse(\%options, $dir);

C<traverse> walks the tree below directory C<$dir>
in the invocant project without the need for a workarea. 
It is modelled on L<File::Find>.

C<&wanted> is a code reference described in 
L</"wanted function"> below. C<$dir>
must be a C<VCS::CMSynergy::Object>. If C<$dir> is omitted,
it defaults to the top level directory of the invocant.

=head3 wanted function

C<&wanted> is called once for all objects below C<$dir> 
including C<$dir> itself. It will also be called on subprojects
of the incocant project, but C<traverse> will not recurse into
subprojects unless the C<subprojects> flag is specified 
(see L</"options"> below).

On each call to C<&wanted>, C<$_> will be bound to the 
currently traversed object (a C<VCS::CMSynergy::Object>). 

C<@VCS::CMSynergy::Traversal::dirs> will be bound to 
an array of C<VCS::CMSynergy::Object>s of cvtype C<dir> representing 
the path  from C<$dir> to C<$_> (in the context of the invocant project).
In particular, C<@VCS::CMSynergy::Traversal::dirs[-1]>
is the parent C<dir> of C<$_>.

The convenience function C<VCS::CMSynergy::Traversal::path()>
returns the filesystem path for C<$_>. It is short for

  join($pathsep, map { $_->name } @VCS::CMSynergy::Traversal::dirs, $_) 

where C<$pathsep> is your platform's path separator.

The convenience function C<VCS::CMSynergy::Traversal::depth()> returns the
current depth, where the top level project has depth 0. It is short for

  scalar @VCS::CMSynergy::Traversal::dirs

Similarly C<@VCS::CMSynergy::Traversal::projects> represents the
subproject hierarchy starting with the invocant project.
In particular, C<$_> is a member of C<$VCS::CMSynergy::Traversal::projects[-1]>.

Note: C<@VCS::CMSynergy::Traversal::dirs> and 
C<@VCS::CMSynergy::Traversal::projects> are both readonly arrays,
i.e. you can't modify them in any way.

You may set C<$VCS::CMSynergy::Traversal::prune> to a true
value in C<&wanted> to stop recursion into sub directories (or subprojects)
(this makes only sense when C<&wanted> is called 
on a C<dir> or C<project> object).

If recursion into subprojects is specfied, C<&wanted>
will be called once for the C<project> object and also for the
top level C<dir> of the subproject.

=head3 options

The first argument of C<traverse> may also be a hash reference.
The following keys are supported:

=over 4

=item C<wanted> (code reference)

The value should be a code reference. It is described in
L</"wanted function">.

=item C<bydepth> (boolean)

If this option is set, C<traverse>
calls C<&wanted> on a directory (or project) only B<after> 
all its entries have been processed. It is "off" by default.

=item C<preprocess> (code reference)

The value should be a code reference. It is used to preprocess
the children of a C<dir> or C<project>, i.e. B<before> L<traverse>
starts traversing it. You can use it to impose an ordering
among "siblings" in the traversal. You can also filter out
objects, so that C<wanted> will never be called on them
(and traversal will not recurse on them in case of
C<dir>s or C<project>s).

The preprocessing function is called with
a list of C<VCS::CMSynergy::Object>s and is expected to return
a possibly reordered subset of this list. Note that
the list may contain C<dir> and C<project> objects.
When the preprocessing function is called,
C<$_> is bound to the parent object (which is always
of C<cvtype> C<dir> or C<project>).

=item C<postprocess> (code reference)

The value should be a code reference. It is invoked just before
leaving the current C<dir> or C<project>.

When the postprocessing function is called,
C<$_> is bound to the current object  (which is always
of C<cvtype> C<dir> or C<project>).

=item C<subprojects> (boolean)

If this option is set, C<traverse>
will recurse into subprojects. It is "off" by default.

=item C<pathsep> (string)

The path separator to use for C<VCS::CMSynergy::Traversal::path()>.
The default is your platform's path separator.

=item C<attributes> (array ref)

This option is only useful if
L<:cached_attributes|VCS::CMSynergy/":cached_attributes"> is in effect.
It should contain a reference to an
array of attribute names. If present, C<traverse> passes it down to
C<query_object> during traversal. Hence all objects encountered
in the traversal (e.g. C<$_> when bound in C<wanted> or the elements
of the directory stack C<@VCS::CMSynergy::Traversal::dirs>) have
their attribute caches primed for the given attributes,
cf. L<query_object|VCS::CMSynergy/"query_object">.

=back

Note that for any particular C<dir> (or C<project>) object,
the above code references are always called in order
C<preprocess>, C<wanted>, C<postprocess>.

Example: 

  my $proj = $ccm->object('toolkit-1.0:project:1');

  $proj->traverse(
    sub { print VCS::CMSynergy::Traversal::path(), "\n" } );

This prints the directory tree of project B<toolkit-1.0:project:1>
similar to the Unix command L<find>. The order of entries in a directory
is unspecified and sub projects are not traversed:

  toolkit
  toolkit/makefile
  toolkit/makefile.pc
  toolkit/misc
  toolkit/misc/toolkit.ini
  toolkit/misc/readme

Another example:

  $proj->traverse(
    {
      wanted => sub {
	return unless $_->cvtype eq "project";
	my $proj_depth = @VCS::CMSynergy::Traversal::projects;
	print "  " x $proj_depth, $_->displayname, "\n";
      },
      preprocess => sub { sort { $a->name cmp $b->name } @_; },
      subprojects => 1,
    });

This prints the complete project hierarchy rooted at  
B<toolkit-1.0:project:1>.  Only projects will be shown,
entries are sorted by name and are intended according to their depth:

  toolkit-1.0
    calculator-1.0
    editor-1.0
    guilib-1.0

=cut

# tied array class that acts as a readonly front to a real array
# NOTE: TIEARRAY expects as first parameter a closure that 
# returns a reference to the "back" array. Storing the array reference 
# itself in the tied arraay doesn't work when the "back" array is local'ized.
{
    package Tie::ReadonlyArray;	

    use Carp;

    sub TIEARRAY	{ bless $_[1], $_[0]; }
    sub FETCH		{ $_[0]->()->[$_[1]]; }
    sub FETCHSIZE	{ scalar @{$_[0]->()}; }
    *STORE = *STORESIZE = *EXTEND = *CLEAR = *UNTIE
	= *PUSH = *POP = *UNSHIFT = *SHIFT = *SPLICE 
	= sub { croak "attempt to modify a readonly array"; };
}


# put some items into the VCS::CMSynergy::Traversal namespace
{
    package VCS::CMSynergy::Traversal;

    # private
    our (@_dirs, @_projects, $_pathsep, $_catdirs);

    # public
    our (@dirs, @projects, $prune);		
    tie @dirs,	   "Tie::ReadonlyArray" => sub { \@_dirs };
    tie @projects, "Tie::ReadonlyArray" => sub { \@_projects };

    # NOTE:references $_ (the currently traversed object)
    sub path		{ return @_dirs ? 
			    $_catdirs.$_pathsep.$_->name : $_->name }

    sub depth		{ return scalar @_dirs }

    sub _catdirs	{ $_catdirs = join($_pathsep, map { $_->name } @_dirs) }
}


my %traverse_opts =
(
    wanted	=> "CODE",
    preprocess	=> "CODE",
    postprocess	=> "CODE",
    attributes	=> "ARRAY",
    bydepth	=> undef,
    subprojects	=> undef,
    pathsep	=> undef,
);

sub traverse
{
    my $self = shift;
    my ($arg_wanted, $dir) = 
        validate(\@_, (CodeRef | HashRef), Optional[InstanceOf["VCS::CMSynergy::Object"]]);

    my %wanted;
    if (ref $arg_wanted eq 'CODE')
    {
	%wanted = ( wanted => $arg_wanted );
    }
    elsif (ref $arg_wanted eq 'HASH')
    {
	%wanted = %$arg_wanted;		# make a copy, so we can't inadvertently modify it
	while (my ($opt, $value) = each %wanted)
	{
	    croak(__PACKAGE__.qq[::traverse: argument 1 ("wanted"): unrecognized option "$opt"])
		unless exists $traverse_opts{$opt};

	    my $type = $traverse_opts{$opt} or next;
	    croak(__PACKAGE__.qq[::traverse: argument 1 ("wanted"): option "$opt" must be a $type: $value])
		unless UNIVERSAL::isa($value, $type);
	}
	croak(__PACKAGE__."::traverse: argument 1 (wanted hash ref): option `wanted' is mandatory")
	    unless $wanted{wanted};
    }

    if (defined $dir)
    {
	croak(__PACKAGE__."::traverse: argument 2 (dir) must have cvtype `dir': $dir")
	    unless $dir->is_dir;

	# check that $dir is indeed a member of $self
        my $parents = $self->has_child($dir);
	return $self->ccm->set_error("directory `$dir' isn't a member of `$self'")
            unless @$parents;

        # fetch its $wanted{attributes}
        $dir->property($wanted{attributes}) if $wanted{attributes};
    }
    else
    {
	$dir = $self;
    }

    local @VCS::CMSynergy::Traversal::_projects = ($self);
    local @VCS::CMSynergy::Traversal::_dirs = (); 
    local $VCS::CMSynergy::Traversal::_pathsep =
	(delete $wanted{pathsep}) || VCS::CMSynergy::Client::_pathsep;

    $self->_traverse(\%wanted, $dir);
}

# helper method: grunt work of traverse
sub _traverse
{
    my ($self, $wanted, $parent) = @_;

    # NOTE: $parent is either a "dir" or "project" by construction
    my $children = $self->is_child_of(
	$parent->is_dir ? $parent : undef, @{ $wanted->{attributes} }) 
	or return;


    if ($wanted->{preprocess})
    {
        # make $_ the current dir/project during preprocess'ing 
	local $_ = $parent;
	{ $children = [ $wanted->{preprocess}->(@$children) ]; }
    }

    if (!$wanted->{bydepth}) 
    {
	local $_ = $parent;
	local $VCS::CMSynergy::Traversal::prune = 0;
	{ $wanted->{wanted}->(); }		# protect against wild "next"
	return 1 if $VCS::CMSynergy::Traversal::prune;
    }

    push @VCS::CMSynergy::Traversal::_dirs, $parent unless $parent->is_project;
    VCS::CMSynergy::Traversal::_catdirs();

    foreach (@$children)			# localizes $_
    {
	if ($_->is_project && $wanted->{subprojects})
	{
	    push @VCS::CMSynergy::Traversal::_projects, $_;
	    $_->_traverse($wanted, $_) or return;
	    pop @VCS::CMSynergy::Traversal::_projects;
	    next;
	}
	if ($_->is_dir)
	{
	    $self->_traverse($wanted, $_) or return;
	    next;
	}

	{ $wanted->{wanted}->(); }
    }

    pop @VCS::CMSynergy::Traversal::_dirs unless $parent->is_project;
    VCS::CMSynergy::Traversal::_catdirs();
    
    if ($wanted->{bydepth}) 
    {
        local $_ = $parent;
        local $VCS::CMSynergy::Traversal::prune = 0;
        { $wanted->{wanted}->(); }
	return 1 if $VCS::CMSynergy::Traversal::prune;
    }

    if ($wanted->{postprocess})
    {
        # make $_ the current dir/project during postprocess'ing 
	local $_ = $parent;
	{ $wanted->{postprocess}->(); }
    }

    return 1;
}


=head2 get_member_info_hashref, get_member_info_object

NOTE: This methods are only useful if you have the optional
Synergy command B<get_member_info> (from the "PC Integrations" package)
installed, cf. F<README.get_member_info> for details.

  $members1 = $proj->get_member_info_hashref(@keywords, \%options);
  $members2 = $proj->get_member_info_object(@keywords, \%options);
 
  while (my ($path, $member) = each %$members2) 
  {
    print "$path $member\n";
  }

C<get_member_info_hashref> and C<get_member_info_object>
execute B<ccm get_member_info> to obtain the members of project C<$proj>.
They both return a reference to a hash where the keys are the 
workarea (relative) pathnames of the members. For C<get_member_info_hashref>,
the value is a hash of attributes similar to L<VCS::CMSynergy/query_hashref>.
For C<get_member_info_object>, the value is the member itself
(a L<CVS::CMSYnergy::Object>), similar to  L<VCS::CMSynergy/query_object>.

If there was an error, C<undef> is returned.

See the description of  L<VCS::CMSynergy/query_hashref> or 
L<VCS::CMSynergy/query_object>, resp., for the meaning of
C<@keywords>. Both methods also accept an optional trailing  
hash reference. Possible keys are:

=over 4

=item C<subprojects> (boolean)

whether to list members of sub projects (recursively), default: false

=item C<pathsep> (string)

separator to use for the workarea pathnames, default: the platform's
native path separator

=back 

Note the following deficiencies inherited from B<ccm get_member_info>:

=over 4

=item *

The member hash does not contain any directories (i.e. Synergy objects
with cvtype "dir"). This usually not a problem since (1) directories
don't carry much information relevant to version control and (2) their
existence is easily inferred from the pathnames. But information
about empty directories will be lost.

=item *

If option C<subprojects> is true the member hash contains 
all members of all sub projects, but doesn't give any information
which sub project a certain member belongs to.

=back

Note the following differences from B<ccm get_member_info>:

=over 4

=item *

Workarea pathnames are always relative (to the top of the workarea),
irrespective whether C<$proj> currently maintains a workarea or not.

=back

=cut

sub get_member_info_hashref
{
    my $self = shift;
    my $opts = @_ && ref $_[-1] eq "HASH" ? pop : {};
    return $self->_get_member_info(\@_, $opts, 0);
}

sub get_member_info_object
{
    my $self = shift;
    my $opts = @_ && ref $_[-1] eq "HASH" ? pop : {};
    return $self->_get_member_info(\@_, $opts, 1);
}

# private method: wrapper for get_member_info from PC integrations intlib.a
# if $row_object is true, returns objects, otherwise hashes
sub _get_member_info
{
    my ($self, $keywords, $opts, $want_row_object) = @_;

    my $want = VCS::CMSynergy::_want($want_row_object, $keywords);

    # NOTE: $RS is at the end (because get_member_info _prepends_ the path)
    my $format = $VCS::CMSynergy::FS . join($VCS::CMSynergy::FS, values %$want) . $VCS::CMSynergy::RS;	

    my @cmd =  qw/get_member_info/;
    push @cmd, "-recurse" if $opts->{subprojects};

    my ($rc, $out, $err) = $self->ccm->_ccm(@cmd, -format => $format, $self);
    return $self->ccm->set_error($err || $out) unless $rc == 0;

    my %result;
    my $wa_path_len = 
	$self->get_attribute("maintain_wa") eq "TRUE" ?
	    length($self->get_attribute("wa_path")) + 1 : 0;

    my $_pathsep = VCS::CMSynergy::Client::_pathsep;

    # split into records
    # NOTE: $RS is followed by \n
    foreach (split(/\Q${VCS::CMSynergy::RS}\E\s*/, $out))		
    {
	my @cols = split(/\Q${VCS::CMSynergy::FS}\E/, $_, -1);

	# path information is the first "column", strip wa_path if necessary
	my $path = shift @cols;	
	substr($path, 0, $wa_path_len) = "" if $wa_path_len;
	$path =~ s/\Q$_pathsep\E/$opts->{pathsep}/g if $opts->{pathsep};

	$result{$path} = $self->ccm->_parse_query_result($want, \@cols, $want_row_object);
    }

    return \%result;
}


=head1 CONVENIENCE METHODS

=head2 recursive_is_member_of, hierarchy_project_members

These are convenience methods to enumerate recursively all members
of the invocant project or just the sub projects.

  $members = $proj->recursive_is_member_of($order_spec, @keywords);
  $sub_projs = $proj->hierarchy_project_members($order_spec, @keywords);

are exactly the same as

  $members = $proj->ccm->query_object(
    "recursive_is_member_of('$proj',$order_spec)", @keywords);
  $sub_projs = $proj->ccm->query_object(
    "hierarchy_project_members('$proj',$order_spec)", @keywords);

C<$order_spec> and C<@keywords> are optional. If C<$order_spec> is
C<undef> or not supplied, C<"none"> is used.
If you supply C<@keywords> these are passed down
to L<VCS::CMSynergy/query_object> as additional keywords.

=cut

sub recursive_is_member_of
{
    my $self = shift;
    my ($order_spec, $keywords) = @_ ? validate(\@_, Maybe[Str], _KEYWORDS) : ();
    $order_spec ||= "none";

    return $self->ccm->query_object("recursive_is_member_of('$self',$order_spec)", @$keywords);
}


sub hierarchy_project_members
{
    my $self = shift;
    my ($order_spec, $keywords) = @_ ? validate(\@_, Maybe[Str], _KEYWORDS) : ();
    $order_spec ||= "none";

    return $self->ccm->query_object("hierarchy_project_members('$self',$order_spec)", @$keywords);
}


=head2 is_child_of, has_child

These are convenience methods to enumerate all members of a directory
(C<is_child_of>) or all directories that contain the object (C<has_child>),
both in the context of the invocant project 

  $members = $proj->is_child_of($dir, @keywords);

  $parents = $proj->has_child($obj, @keywords);

are exactly the same as

  $members = $proj->ccm->query_object(
    "is_child_of('$dir','$proj')", @keywords);

  $parents = $proj->ccm->query_object(
    "has_child('$obj','$proj')", @keywords);

For C<has_child>, C<$obj> may be any C<VCS::CMSynergy::Object>.

For C<is_child_of>, C<$dir> is optional; if supplied
it must be a C<VCS::CMSynergy::Object> of type C<"dir">.
If C<$dir> is C<undef> or not supplied, C<is_child_of> returns
the toplevel directory of the invocant project (NOTE: the return value
is actually a reference to an array with one element).

If you supply C<@keywords> these are passed down
to L<VCS::CMSynergy/query_object> as additional keywords.

=cut

sub is_child_of
{
    my $self = shift;
    my ($dir, $keywords) = 
        validate(\@_, Maybe[InstanceOf["VCS::CMSynergy::Object"]], _KEYWORDS);
    if (defined $dir)
    {
	croak(__PACKAGE__."::is_child_of: argument 1 ($dir) must have cvtype `dir'")
	    unless $dir->is_dir;
    }
    else
    {
	$dir = $self;
    }

    return $self->ccm->query_object("is_child_of('$dir','$self')", @$keywords);
}

sub has_child
{
    my $self = shift;
    my ($obj, $keywords) = 
        validate(\@_, InstanceOf["VCS::CMSynergy::Object"], _KEYWORDS);

    return $self->ccm->query_object("has_child('$obj','$self')", @$keywords);
}


=head2 object_from_path

  $obj = $proj->object_from_path($path, @keywords);
  $obj = $proj->object_from_path(\@path_components, @keywords);

Returns the C<VCS::CMSynergy::Object> identified by
workarea path C<$path> in project C<$proj>.

  $obj = $proj->ccm->object_from_proj_ref($path, $proj, @keywords);
  $obj = $proj->ccm->object_from_proj_ref(\@path_components, $proj, @keywords);

See L<VCS::CMSynergy/object_from_proj_ref> for details.

=cut

sub object_from_path
{
    my $self = shift;
    my ($path, $keywords) = validate(\@_, (Str | ArrayRef[Str]), _KEYWORDS);

    return $self->ccm->object_from_proj_ref($path, $self, @$keywords);
}


=head2 project_tree

  $hash = $proj->project_tree(\%options);

is exactly the same as

  $hash = $proj->ccm->project_tree(\%options, $proj);

See L<VCS::CMSynergy/project_tree>.

=cut

sub project_tree
{
    my ($self, $options) = @_;
    return $self->ccm->project_tree($options, $self);
}

=head2 top_dir

  $dir = $proj->top_dir(@keywords);

Returns the C<VCS::CMSynergy::Object> representing the top level directory
of project C<$proj>.

If you supply C<@keywords> these are passed down
to L<VCS::CMSynergy/query_object> as additional keywords.

=cut

sub top_dir
{
    my ($self, @keywords) = @_;
    return $self->ccm->query_object(
        { is_child_of => [ $self, $self ] }, @keywords)->[0];
}

 
=head1 MISCELLANEOUS

=head2 show_reconfigure_properties

  $objects = $proj->show_reconfigure_properties($what, @keywords, \%options);

Shows information about the project's reconfigure properties
depending on C<$what>. C<@keywords> and C<\%options> are optional.
Returns a reference to an array of C<VCS::CMSynergy::Objects>.

C<$what> must be one of the following strings:

=over 4

=item C<"tasks">

shows tasks that are directly in the project's reconfigure properties

=item C<"folders">

shows folders that are in the project's reconfigure properties

=item C<"tasks_and_folders">

shows tasks and folders that are directly in the project's 
reconfigure properties

=item C<"all_tasks">

shows all tasks that are directly or indirectly in the project's 
reconfigure properties (indirectly means the task is in a folder 
that is in the project's reconfigure properties) 

=item C<"objects">

shows objects in the task that are either directly or indirectly
in the project's reconfigure properties

=back

See the description of  L<VCS::CMSynergy/query_hashref> or 
L<VCS::CMSynergy/query_object>, resp., for the meaning of 
C<@keywords>. 

C<show_reconfigure_properties> also accepts an optional trailing  
hash reference. Possible keys are:

=over 4

=item C<subprojects> (boolean)

whether to include the reconfigure properties 
of sub projects (recursively), default: false

=item C<automatic> (boolean)

whether automatic tasks are to be shown, default: false; 
this option is only relevant if C<$what> is "tasks", "tasks_and_folders" 
or "all_tasks"

=back

Example: 

  $tasks = $proj->show_reconfigure_properties(
	     all_tasks => qw/task_synopsis completion_date/, 
	     { subprojects => 1, automatic => 0 });

=cut 

sub show_reconfigure_properties
{
    my $self = shift;
    my $opts = @_ && ref $_[-1] eq "HASH" ? pop : {};
    my ($what, $keywords) = validate(\@_, Str, _KEYWORDS);

    croak(__PACKAGE__."::show_reconfigure_properties:".
	  " argument 1 (what) must be one of tasks|folders|tasks_and_folders|all_tasks|objects")
	unless $what =~ /^(tasks|folders|tasks_and_folders|all_tasks|objects)$/;

    my $want = VCS::CMSynergy::_want(1, $keywords);
    my $format = $VCS::CMSynergy::RS . join($VCS::CMSynergy::FS, values %$want) . $VCS::CMSynergy::FS;

    my @cmd = qw/reconfigure_properties -u -ns/;
    push @cmd, $opts->{automatic} ? "-auto" : "-no_auto" if $what =~ /tasks/;
    push @cmd, "-r" if $opts->{subprojects};

    my ($rc, $out, $err) = $self->ccm->_ccm(
	@cmd, -format => $format, -show => $what, $self);
    return $self->ccm->set_error($err || $out) unless $rc == 0;
    # NOTE: if the reconf properties are empty, Synergy shows the string "None"
    return [ ] if $out eq "None";

    my @result;
    foreach (split(/\Q${VCS::CMSynergy::RS}\E/, $out))	# split into records 
    {
	next unless length($_);				# skip empty leading record
	my @cols = split(/\Q${VCS::CMSynergy::FS}\E/, $_, -1);	# don't strip empty trailing fields
	push @result, $self->ccm->_query_result($want, \@cols, 1);
    }
    return \@result;
}


1;
