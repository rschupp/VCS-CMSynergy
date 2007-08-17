package VCS::CMSynergy::Users;

our $VERSION = sprintf("%d.%02d", q%version: 1.05 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy::Users - Perl interface to CM Synergy user administration

=head1 SYNOPSIS

  use VCS::CMSynergy::Users;

  $hash_ref = $ccm->users;
  $ccm->users(\%user_roles);
  $ccm->add_user($user, @roles);
  $ccm->delete_user($user);
  @roles = $ccm->get_roles($user);
  $ccm->add_roles($user, @roles);
  $ccm->delete_roles($user, @roles);

=head1 DESCRIPTION

NOTE: This interface is subject to change.

  use VCS::CMSynergy;
  use VCS::CMSynergy::Users;

  my $ccm = VCS::CMSynergy->new(database => "/ccmdb/test/tut62/db");

  $ccm->add_user('jluser', qw(developer build_mgr));

=head1 METHODS

=over 4

=cut

package VCS::CMSynergy;

=item users

  $hash_ref = $ccm->users;
  $ccm->users($hash_ref);

The first form gets the table of users and their roles as a hash ref.
The keys are the user names and the values are array refs containing
the user's roles, e.g.

  $hash_ref = {
    'jluser'	=> [ qw(developer) ],
    'psizzle'	=> [ qw(developer build_mgr) ],
    ...
    };

You can use this form with any role (because it is I<not> implemented
via B<ccm users>).

The second form replaces the existing table of users and their roles
with the contents of C<$hashref>. You must have the B<ccm_admin> role
to use this form. For typical CM Synergy administrator usage
it is usually more convenient to use one of the methods below.

=cut

sub users
{
    my $self = shift;
    return $self->set_error("too many arguments") unless @_ <= 1;

    # NOTE: For getting the list of users we use 
    # "ccm attr -show users base-1:model:base" because every role can do that -
    # whereas "ccm users" requires the ccm_admin role.
    if (@_ == 0)
    {
	my $attr = $self->attribute('users', $self->object(qw(base 1 model base)));
	return undef unless defined $attr;
	
	my $users = {};
	foreach (split(/\n/, $attr))
	{
	    my ($user, $roles) = /^ \s* user \s+ (\S+) \s* = \s* (.*) ;/x;
	    next unless defined $user;
	    $users->{$user} = [ split(' ', $roles) ];
	}

	return $users;
    }

    my $users = shift;
    return $self->set_error("illegal type of argument") 
	unless ref $users eq "HASH";

    # use ye olde text_editor trick
    my ($fh, $tempfile) = tempfile();
    {
	local $" = ' ';			# paranoia setting
	print $fh "user $_ = @{ $users->{$_} };\n" foreach (keys %$users);
    }
    close($fh);

    $self->_with_local_option(
	text_editor => "$Config{cp} $tempfile %filename",
	qw(ccm users));
	# FIXME error handling?
    unlink($tempfile);

    return $users;
}

=item C<add_user>

  $ccm->add_user($user, @roles);

Adds the user with the given roles. 
If the user already exists her roles will be reset to the ones given.

=cut

sub add_user
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return undef unless $users;

    $users->{$user} = [ @roles ];

    $self->users($users);
}

=item C<delete_user>

  $ccm->delete_user($user);

Deletes the user. No error is signalled if the user doesn't exist.

=cut

sub delete_user
{
    my ($self, $user) = @_;

    my $users = $self->users;
    return undef unless $users;

    delete $users->{$user};

    $self->users($users);
}

=item C<add_roles>

  $ccm->add_roles($user, @roles);

Grants the given roles to the user. It is no error if the user
already has some of the given rules.

=cut

sub add_roles
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return undef unless $users;

    return $self->set_error("user `$user' doesn't exist")
	unless exists $users->{$user};

    my %roles;
    $roles{$_}++ foreach (@{ $users->{$user} }, @roles);
    $users->{$user} = [ keys %roles ];

    $self->users($users);
}

=item C<delete_roles>

  $ccm->delete_roles($user, @roles);

Revokes the given roles from the user. It is no error if the
user doesn't have any of the given rules.

=cut

sub delete_roles
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return undef unless $users;

    return $self->set_error("user `$user' doesn't exist")
	unless exists $users->{$user};

    my %roles;
    $roles{$_}++ foreach (@{ $users->{$user} });
    delete $roles{$_} foreach (@roles);

    $users->{$user} = [ keys %roles ];

    $self->users($users);
}

=item C<get_roles>

  @roles = $ccm->get_roles($user);

Returns the roles for the user. Returnis C<undef> if the user doesn't exist.

=cut

sub get_roles
{
    my ($self, $user) = @_;

    my $users = $self->users;
    return undef unless $users && exists $users->{$user};

    return @{ $users->{$user} };
}

=back

1;
