package VCS::CMSynergy::Project;

our $VERSION = do { (my $v = q%version: 5 %) =~ s/.*://; sprintf("%d.%02d", split(/\./, $v), 0) };

=head1 NAME

VCS::CMSynergy::Project - FIXME

=head1 SYNOPSIS

=cut 

use strict;

use base qw(VCS::CMSynergy::Object);

use Carp;
use VCS::CMSynergy::Client qw(_usage);
use File::Spec;
use Cwd;

# FIXME make set_error a V::C::O method?

sub recursive_is_member_of
{
    my $self = shift;
    return $self->ccm->query_object("recursive_is_member_of('$self',depth)", @_);
}


sub hierarchy_project_members
{
    my $self = shift;
    return $self->ccm->query_object("hierarchy_project_members('$self',depth)", @_);
}


# NOTE: $dir_object is undef => returns project's top level dir
# FIXME needs test
sub is_child_of
{
    _usage(1, undef, '[{ $dir_object | undef }, @keywords]', \@_);
    my ($self, $dir) = splice @_, 0, 2;

    if (defined $dir)
    {
	croak(__PACKAGE__."::is_child_of: argument 1 ($dir) must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	croak(__PACKAGE__."::is_child_of: argument 1 ($dir) must have cvtype `dir'")
	    unless $dir->is_dir;
    }
    else
    {
	$dir = $self;
    }
    return $self->ccm->query_object("is_child_of('$dir','$self')", @_);
}


# create V::C::O from a "project reference",
# i.e. a project and a relative path
# NOTE: takes either string (path) or array ref of path components
# FIXME needs test
sub object_from_proj_ref
{
    _usage(2, undef, '{ $path | \\@path_components }, @keywords', \@_);
    my ($self, $path) = splice @_, 0, 2;

    return $self->ccm->object_from_proj_ref($path, $self);
}


# NOTE return undef on failure (no wa maintained, chdir failed etc), old pwd otherwise
# FIXME needs test
sub chdir_into_wa
{
    my $self = shift;
    return $self->ccm->set_error("project `$self´ doesn't maintain a workarea")
	unless $self->get_attribute("maintain_wa") eq "TRUE";

    my $wa_top = File::Spec->catfile($self->get_attribute("wa_path"), $self->name);
    my $old_pwd = cwd();
    chdir($wa_top) 
	or return $self->ccm->set_error("can't chdir($wa_top) into workarea of project `$self': $!");
    return $old_pwd;
}


# tied array class that acts as a readonly front to a real array
{
    package Tie::ReadonlyArray;	

    use Carp;

    sub TIEARRAY	{ bless $_[1], $_[0]; }
    sub FETCH		{ $_[0]->[$_[1]]; }
    sub FETCHSIZE	{ scalar @{$_[0]}; }
    sub AUTOLOAD	{ croak "attempt to modify a readonly array"; }
}


# put some items into the VCS::CMSynergy::Traversal namespace
{
    package VCS::CMSynergy::Traversal;

    our (@_dirs, @_projects);			# private
    our $_pathsep = $^O eq "MSWin32" ? "\\" : "/" ;

    our (@dirs, @projects, $prune);		# public
    tie @dirs,		"Tie::ReadonlyArray" => \@_dirs;
    tie @projects,	"Tie::ReadonlyArray" => \@_projects;

    sub path 
    { 
	my ($pathsep) = @_;
	$pathsep = $_pathsep unless defined $pathsep;

	return join($pathsep, map { $_->name } 
	                          @VCS::CMSynergy::Traversal::_dirs, $_); 
    }

    sub depth 
    { 
	return scalar @VCS::CMSynergy::Traversal::_dirs; 
    }
}


my %traverse_opts =
(
    wanted	=> "CODE",
    preprocess	=> "CODE",
    postprocess	=> "CODE",
    attributes	=> "ARRAY",
);

sub traverse
{
    _usage(2, 3, '{ \\&wanted | \\%wanted } [, $dir_object]', \@_);
    my ($self, $wanted, $dir) = @_;

    if (ref $wanted eq 'CODE')
    {
	$wanted = { wanted => $wanted };
    }
    elsif (ref $wanted eq 'HASH')
    {
	croak(__PACKAGE__."::traverse: argument 1 (wanted hash ref): option `wanted' is mandatory")
	    unless exists $wanted->{wanted};
	while (my ($key, $type) = each %traverse_opts)
	{
	    next unless exists $wanted->{$key};
	    croak(__PACKAGE__."::traverse: argument 1 (wanted hash ref): option `$key' must be a $type: $wanted->{$key}")
		unless UNIVERSAL::isa($wanted->{$key}, $type);
	}
    }
    else
    {
	croak(__PACKAGE__."::traverse: argument 1 (wanted) must be a CODE or HASH ref: $wanted");
    }
    $wanted->{attributes} ||= [];

    if (defined $dir)
    {
	croak(__PACKAGE__."::traverse: argument 2 (dir) must be a VCS::CMSynergy::Object: $dir")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	croak(__PACKAGE__."::traverse: argument 2 (dir) must have cvtype `dir': $dir")
	    unless $dir->is_dir;

	# check that $dir is member of $self
	# FIXME there must be a better way to do this
	my $result = $self->ccm->query_object(
	    {
		name		=> $dir->name,
		cvtype		=> $dir->cvtype,
		instance	=> $dir->instance,
		version		=> $dir->version,
		is_member_of	=> [ $self ]
	    },
	    @{ $wanted->{attributes} });
	return $self->ccm->set_error("directory `$dir' doesn't exist or isn't a member of `$self'")
	    unless @$result;
	$dir = $result->[0];
    }
    else
    {
	$dir = $self;
    }

    local @VCS::CMSynergy::Traversal::_projects = ($self);
    local @VCS::CMSynergy::Traversal::_dirs = (); 
    $self->_traverse($wanted, $dir);
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


1;
