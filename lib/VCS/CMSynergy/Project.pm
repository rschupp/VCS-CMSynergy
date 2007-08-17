package VCS::CMSynergy::Project;

our $VERSION = do { (my $v = q%version: 2 %) =~ s/.*://; sprintf("%d.%02d", split(/\./, $v), 0) };

=head1 NAME

VCS::CMSynergy::Project - FIXME

=head1 SYNOPSIS

=cut 

use strict;

use base qw(VCS::CMSynergy::Object);

use Carp;
use VCS::CMSynergy::Client qw(_usage ROW_OBJECT);
use File::Spec;
use Cwd;

# FIXME make set_error a V::C::O method?

sub recursive_is_member_of
{
    my $self = shift;
    return $self->ccm->query_object_with_attributes(
	"recursive_is_member_of('$self',depth)", @_);
}


sub hierarchy_project_members
{
    my $self = shift;
    return $self->ccm->query_object_with_attributes(
	"hierarchy_project_members('$self',depth)", @_);
}


# NOTE: no $dir_object specified => returns project's top level dir
# FIXME needs test
sub is_child_of
{
    _usage(1, undef, '[$dir_object, ] $keyword...', \@_);
    my $self = shift;
    my $dir;
    if (@_ && ref $_[0])
    {
	$dir = shift @_;
	return $self->ccm->set_error("argument `$dir' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	return $self->ccm->set_error("argument `$dir' must have cvtype `dir'")
	    unless $dir->is_dir;
    }
    else
    {
	$dir = $self;
    }
    return $self->ccm->query_object_with_attributes(
	"is_child_of('$dir','$self')", @_);
}


# NOTE: takes either string (relative path) or array ref of path components
# FIXME needs test
sub path_to_object
{
    _usage(2, undef, '{ $path | \\@path_components }, @keywords', \@_);
    my ($self, $path, @keywords) = @_;

    $path = join("/", @$path) if ref $path; # FIXME use native path delim here

    return $self->ccm->_property(
	"$path\@$self", [ object => @keywords ], ROW_OBJECT);
    # NOTE/FIXME: no error if path isn't bound? possible errors:
    # Specified project not found in database: '$self'
    # Object version could not be identified from reference form: '$path'
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
	return $self->ccm->set_error("argument 1: option `wanted' is mandatory")
	    unless exists $wanted->{wanted};
	while (my ($key, $type) = each %traverse_opts)
	{
	    next unless exists $wanted->{$key};
	    return $self->ccm->set_error("argument 1: option `$key' must be a $type")
		unless UNIVERSAL::isa($wanted->{$key}, $type);
	}
    }
    else
    {
	return $self->ccm->set_error("argument 1 must be a CODE or HASH ref");
    }
    $wanted->{attributes} ||= [];

    if (defined $dir)
    {
	return $self->ccm->set_error("argument 2 `$dir' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	return $self->ccm->set_error("argument 2 `$dir' must have cvtype `dir'")
	    unless $dir->is_dir;

	# check that $dir is member of $self
	# FIXME there must be a better way to do this
	my $result = $self->ccm->query_object_with_attributes(
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

    # NOTE/FIXME: can't use $self->is_child_of($parent) here 
    # (because $parent might be not a dir (but a project))
    my $children = $self->ccm->query_object_with_attributes(
	{ is_child_of => [ $parent, $self ] },
	@{ $wanted->{attributes} }) or return;


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
