package VCS::CMSynergy::Helper;

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
require Getopt::Long;
use Carp;
=======
our $VERSION = sprintf("%d.%02d", q%version: 1.20 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy::Helper - ancillary convenience functions
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

=head1 SYNOPSIS

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
  CM Synergy Options:
=======
  my @ccm_opts = VCS::CMSynergy::Helper::GetOptions;
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
  -D PATH | --database PATH       database path
  -H HOST | --host HOST           engine host
  -U NAME | --user NAME           user name
  -P STRING | --password STRING   user's password
  --ui_database_dir PATH          path to copy database information to
=======
=head2 GetOptions

This function extracts a set of common options from C<@ARGV> and converts
them to the corresponding options for L<VCS::CMSynergy/new>.
All options iand arguments in C<@ARGV> that it doesn't know about 
are left untouched. 
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
=head1 C<GetOptions>
=======
It may be used to make all your CM Synergy scripts accept a
uniform set of options:
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

  use Getopt::Long;
  use VCS::CMSynergy;
  use VCS::CMSynergy::Helper;
  ...

  # extract CM Synergy options from @ARGV
  my @ccm_opts = VCS::CMSynergy::Helper::GetOptions;

  # process other options in @ARGV
  GetOptions(...);

  # start CM Synergy session
  my $ccm = VCS::CMSynergy->new(
      @ccm_opts,
      RaiseError => 1,
      PrintError => 0);
<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
=======
  ...
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
All single letter options are in uppercase so that
scripts using C<VCS::CMSynergy::Helper::GetOptions> still
can use all lowercase letters for their own options.
=======
The following options are recognized:
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

=over 4

=item c<-D>, C<--database>

absolute database path; this option corresponds to option C<database>
for C<VCS::CMSynergy/start>

=item c<-H>, C<--host>

engine host; this option corresponds to option C<host> for
for C<VCS::CMSynergy/start>

=item c<-U>, C<--user>

user; this option corresponds to option C<user>
for C<VCS::CMSynergy/start>

=item C<-P>, C<--password>

user's password; this option corresponds to option C<password> for
for C<VCS::CMSynergy/start>

=item C<--ui_database_dir>

path name to which your database information is copied when you are 
running a remote client session; this option corresponds to 
option C<ui_database_dir> for B<ccm start>, it implies C<remote_client>

=back

If no database was specified C<GetOptions> adds 

  CCM_ADDR => $ENV{CCM_ADDR}

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
to the returned hash. It will L<carp|Carp/carp> if $ENV{CCM_ADDR}
=======
to the returned hash. It will L<croak|Carp/croak> if $ENV{CCM_ADDR}
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807
is not defined.

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
=======
Note that all recognized single letter options are in uppercase so that
scripts using C<VCS::CMSynergy::Helper::GetOptions> still
can use all lowercase letters for their own options.

Here's the short description of recognized options that you 
can cut and paste into your script's POD documentation:

  CM Synergy Options:

  -D PATH | --database PATH       database path
  -H HOST | --host HOST           engine host
  -U NAME | --user NAME           user name
  -P STRING | --password STRING   user's password
  --ui_database_dir PATH          path to copy database information to

>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807
=cut

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
=======
require Getopt::Long;
use Carp;

>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807
sub GetOptions()
{
    my %opts;
    
<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
    Getopt::Long::Configure('passthrough');
=======
    Getopt::Long::Configure(qw(passthrough no_ignore_case));
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

    Getopt::Long::GetOptions(\%opts,
	'database|D=s',
	'host|H=s',
	'user|U=s',
	'password|P=s',
	'ui_database_dir=s');
    $opts{remote_client} = 1 if exists $opts{ui_database_dir};

<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
    Getopt::Long::Configure('no_passthrough');
=======
    Getopt::Long::Configure(qw(no_passthrough));

>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807

    unless (defined $opts{database})
    {
<<<<<<< /ccmdb/scm/support/st_root/cache/source/#07/29807
	unless (defined ($opts{CCM_ADDR} = $ENV{CCM_ADDR}))
	{
	    carp("no database specified and CCM_ADDR not set");
	    return;
	}
=======
	# default CCM_ADDR from environment if no --database was specified;
	# croak if neither was specified
	$opts{CCM_ADDR} = $ENV{CCM_ADDR} or
	    croak "Don't know how to connect to CM Synergy:\nneither CCM_ADDR set in environment, nor database specified in options (with -D or --database)\n";
>>>>>>> /ccmdb/scm/support/st_root/cache/source/#07/29807
    }

    return %opts;
}

1;
