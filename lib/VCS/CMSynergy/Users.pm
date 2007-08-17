package VCS::CMSynergy::Users;

our $VERSION = do { (my $v = q%version: 9 %) =~ s/.*://; sprintf("%d.%02d", split(/\./, $v), 0) };

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

=cut

package VCS::CMSynergy;

=head2 users

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

You need not be in the I<ccm_admin> role to use this form
(because it is I<not> implemented via B<ccm users>).

The second form replaces the existing table of users and their roles
with the contents of C<$hashref>. Duplicate roles will be removed
from C<$hashref>'s values before writing back the table.
You must be in the B<ccm_admin> role to use this form. 

Note that

  $ccm->users($ccm->users);

results in a functionally equivalent, though probably not identical
users table as before (due to hash key ordering etc).

Note: For typical CM Synergy administrator usage
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
	my $text = $self->get_attribute(users => $self->base_model);
	return unless defined $text;
	
	my $users = {};
	foreach (split(/\n/, $text))
	{
	    my ($user, $roles) = /^ \s* user \s+ (\S+) \s* = \s* (.*) ;/x;
	    next unless defined $user;
	    my @roles =  split(" ", $roles);
	    $users->{$user} = \@roles;
	}

	return $users;
    }

    my $users = shift;
    return $self->set_error("illegal type of argument (hash ref expected)") 
	unless ref $users eq "HASH";

    my $text = "";
    foreach my $user (sort keys %$users)
    {
	my $roles = $users->{$user};
	return $self->set_error("illegal value for user `$user' (array ref expected)")
	    unless ref $roles eq "ARRAY";
	return $self->set_error("no roles defined for user `$user'")
	    unless @$roles;

	# remove duplicates
	my %roles = map { $_ => 1 } @$roles;

	$text .= "user $user = " . join(" ", sort keys %roles) . ";\n";
    }

    my ($rc, $out, $err) = $self->ccm_with_text_editor($text, qw(users));
    return $self->set_error($err || $out) unless $rc == 0;
    return $users;
}

=head2 add_user

  $ccm->add_user($user, @roles);

Adds the user with the given roles. 
If the user already exists her roles will be reset to the ones given.

=cut

sub add_user
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return unless $users;

    $users->{$user} = \@roles;

    $self->users($users);
}

=head2 delete_user

  $ccm->delete_user($user);

Deletes the user. No error is signalled if the user doesn't exist.

=cut

sub delete_user
{
    my ($self, $user) = @_;

    my $users = $self->users;
    return unless $users;

    delete $users->{$user};

    $self->users($users);
}

=head2 add_roles

  $ccm->add_roles($user, @roles);

Grants the given roles to the user. It is no error if the user
already has some of the given rules.

=cut

sub add_roles
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return unless $users;

    return $self->set_error("user `$user' doesn't exist")
	unless $users->{$user};

    push @{ $users->{$user} }, @roles;

    $self->users($users);
}

=head2 delete_roles

  $ccm->delete_roles($user, @roles);

Revokes the given roles from the user. It is no error if the
user doesn't have any of the given rules.

=cut

sub delete_roles
{
    my ($self, $user, @roles) = @_;

    my $users = $self->users;
    return unless $users;

    return $self->set_error("user `$user' doesn't exist")
	unless exists $users->{$user};
 
    my %to_be_deleted = map { $_ => 1 } @roles;
    @{ $users->{$user} } = grep { !$to_be_deleted{$_} } @{ $users->{$user} };

    $self->users($users);
}

=head2 get_roles

  @roles = $ccm->get_roles($user);

Returns the roles for the user. Returns an empty list
if the user doesn't exist.

=cut

sub get_roles
{
    my ($self, $user) = @_;

    my $users = $self->users;
    return unless $users;

    my $roles = $users->{$user};
    return $roles ? @$roles : ();
}

=head1 SEE ALSO

L<VCS::CMSynergy> 
L<VCS::CMSynergy::Object>, 
L<VCS::CMSynergy::Client>,

=head1 AUTHORS

Roderich Schupp, argumentum GmbH <schupp@argumentum.de>

=head1 COPYRIGHT AND LICENSE

The VCS::CMSynergy::Users module is Copyright (c) 2001-2005 argumentum GmbH, 
L<http://www.argumentum.de>.  All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=cut

1;
