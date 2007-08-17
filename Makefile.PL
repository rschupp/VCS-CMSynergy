use ExtUtils::MakeMaker;
use File::Spec;
use strict;

# --- Check that CM Synergy CLI is installed

die qq{  
  The CCM_HOME environment variable must be set.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless $ENV{CCM_HOME};

my $ccm_exe = File::Spec->catfile(
    $ENV{CCM_HOME}, 'bin', $^O eq 'MSWin32' || $^O eq 'cygwin' ? 'ccm.exe' : 'ccm');
die qq{  
  The value of the CCM_HOME environment variable
  ($ENV{CCM_HOME}) is not valid.
  It must be set to hold the path to a CM Synergy installation directory
  on this machine. See the README file for more information.
  ABORTED!
} unless -x $ccm_exe;

my $ccm_version = qx($ccm_exe version);
die qq{  
  The CM Synergy CLI programm ($ccm_exe)
  doesn't appear to work on this machine. 
  See the README file for more information.
  ABORTED!
} unless $? == 0;

$ccm_version =~ /^(?:Continuus|CM Synergy) Version\s+(\d+\.\d+)/i;
print "Detected CM Synergy version $1\n";

warn "VCS::CMSynergy likes to have Memoize.pm, but doesn't require it.\n"
    unless eval { require Memoize; 1 };


# --- Create Makefile

WriteMakefile(
    NAME		=> 'VCS::CMSynergy',
    AUTHOR		=> 'Roderich Schupp <schupp@argumentum.de>',
    VERSION_FROM	=> 'CMSynergy.pm',
    PREREQ_PM		=> {
    #			     'Expect'		=> '1.15',
			     'File::Spec'	=> 0,
			     'File::Temp'	=> 0,
			     'Test::More'	=> 0, 	# for `make test' only
			   },
    dist		=> { 
			     COMPRESS		=> 'gzip -9f', 
			     SUFFIX		=> 'gz' 
			   },
    realclean		=> { FILES		=> 't/zz_*.t', },
);

sub MY::postamble
{
    # arrange for all tests to be run a second time with UseCoprocess => 1
    # (but only if we can find Expect.pm)
    eval { require Expect; import Expect 1.15; };
    return if $@;

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
