#!/usr/bin/perl -w

# %version: 14 %

use ExtUtils::MakeMaker;
use Config;
use strict;

# --- Check that CM Synergy CLI is installed

die qq{  
  The CCM_HOME environment variable must be set.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless $ENV{CCM_HOME};

my $ccm_exe = MY->catfile(
    $ENV{CCM_HOME}, "bin", "ccm$Config{_exe}");
die qq{  
  The value of the CCM_HOME environment variable
  ($ENV{CCM_HOME}) is not valid.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless -x $ccm_exe || ($^O eq 'cygwin' && -e $ccm_exe);
# NOTE: -x $ccm_exe fails on cygwin

my $ccm_version = qx("$ccm_exe" version);
die qq{  
  The CM Synergy CLI programm ($ccm_exe)
  doesn't appear to work on this machine. 
  See the README file for more information.
  ABORTED!
} unless $? == 0;

$ccm_version =~ /^(?:Continuus|CM Synergy) Version\s+(\d+\.\d+)/i;
print "Detected CM Synergy version $1\n";

warn qq{
  Please install module Scalar::Util and make sure it implements weaken().
  Without weaken(), CM Synergy session might not be stopped as soon
  as the corresponding VCS::CMSynergy object goes out of scope
  (but not that all sessions will be stopped when perl exits anyway). 
  Cf. "Why is Scalar::Util recommended?" in the POD for 
  VCS::CMSynergy::Object.pm for details.
} unless (eval "use Scalar::Util qw(weaken); 1");


# --- Create Makefile

my %prereq =
(
    'File::Spec'		=> 0,
    'File::Temp'		=> 0,
    'IPC::Run3'			=> 0.01,
    'Time::HiRes'		=> 0,
    'Test::More'		=> 0,	 	# for `make test' only
);
$prereq{'Filesys::CygwinPaths'} = 0 if $^O eq 'cygwin';

WriteMakefile(
    NAME		=> 'VCS::CMSynergy',
    AUTHOR		=> 'Roderich Schupp <schupp@argumentum.de>',
    VERSION_FROM	=> 'lib/VCS/CMSynergy.pm',
    PREREQ_PM		=> \%prereq,
    dist		=> { 
	COMPRESS	=> 'gzip -9f', 
	SUFFIX		=> 'gz' 
    },
    realclean		=> { FILES => 't/zz_*.t' },
);

package MY;

sub postamble
{
    my $self = shift;

    # arrange for all tests to be run a second time with UseCoprocess => 1
    # (but only if we can find Expect.pm)
    if (eval { require Expect; import Expect 1.15; 1; })
    {
	print "Detected suitable version of Expect.pm, enabling tests with UseCoprocess => 1\n";
	opendir DIR, 't';
	my @tests = grep { /\.t$/ } readdir DIR;
	closedir DIR;
	foreach my $test (@tests) 
	{
	    next if $test =~ /^zz_.*\.t$/;
	    $test =~ s/\.t$//;
	    my $zz_test = "t/zz_${test}.t";
	    open ZZ, ">$zz_test" or warn "Can't create $zz_test: $!";
	    print ZZ "#!perl -w\n";
	    print ZZ "\$ENV{CCM_USE_COPROCESS}=1;\n";
	    print ZZ "do 't/$test.t' or warn \$!;\n";
	    print ZZ "die if \$\@;\n";
	    print ZZ "exit 0;\n";
	    close ZZ or warn "Error writing $zz_test: $!";
	}
    }

    return "";
}

sub post_initialize
{
    my $self = shift;

    # don't install Changes.pod as man page
    delete $self->{MAN3PODS}->{'lib/VCS/CMSynergy/Changes.pod'};

    return "";
}
