use ExtUtils::MakeMaker;
use File::Spec;
use strict;

# --- Check that CM Synergy CLI is installed

die qq{  The CCM_HOME environment variable must be set.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless $ENV{CCM_HOME};

my $ccm_exe = File::Spec->catfile(
    $ENV{CCM_HOME}, 'bin', $^O eq 'MSWin32' || $^O eq 'cygwin' ? 'ccm.exe' : 'ccm');
die qq{  The value of the CCM_HOME environment variable
  ($ENV{CCM_HOME}) is not valid.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless -x $ccm_exe;

my $ccm_version = qx($ccm_exe version);
die qq{  The CM Synergy CLI programm ($ccm_exe)
  doesn't appear to work on this machine. 
  See the README file for more information.
  ABORTED!
} unless $? == 0;
$ccm_version =~ /^(?:Continuus|CM Synergy) Version\s+(\d+\.\d+)/i;
print "Detected CM Synergy version $1\n";


# --- Create Makefile

WriteMakefile(
    NAME		=> 'VCS::CMSynergy',
    AUTHOR		=> 'Roderich Schupp <schupp@argumentum.de>',
    VERSION_FROM	=> 'CMSynergy.pm',
    PREREQ_PM		=> {
    #			     'Expect'		=> '1.15',
			     'File::Spec'	=> 0,
			     'File::Temp'	=> 0,
			     'Memoize'		=> 0,
			   },
    dist		=> { 
			     COMPRESS => 'gzip -9f', 
			     SUFFIX => 'gz' 
			   },
);
