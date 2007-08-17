#!/usr/bin/perl -w

use Test::More tests => 9;
use t::util;
use Cwd;
use File::Path;
use File::Spec;

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# make sure there are no remnants from previous tests
rmtree("calculator-test", 0, 0);

my $pwd = getcwd();

ok($ccm->checkout(qw(-project calculator-1.0 -not_copy_based -to test -path), $pwd),
   q[checkout project calculator-1.0 to calculator-test]);

eval 		# so that we clean up even in case of errors
{
    my $ccmwaid = File::Spec->catfile(
	qw(calculator-test calculator),
        $VCS::CMSynergy::Is_MSWin32 ? "_ccmwaid.inf" : ".ccmwaid.inf");
    ok(-e $ccmwaid, qq[check for ccmwaid file ($ccmwaid)]);

    # chdir to project sub directory (esp. for testing coprocess)
    ok(chdir(File::Spec->catdir(qw(calculator-test calculator sources))), 
       q[chdir to project sub directory (sources)]);
    ok(-e "clear.c", q[check for file there]);

    # use workarea name to specify an object
    my $mtime;
    ok($mtime = $ccm->get_attribute(modify_time => "clear.c"),
       q[get_attribute via workarea name]);
    is($mtime, "1999-11-17 04:23:11", q[check attribute value]);
};

# cleanup 
chdir($pwd);
ok($ccm->delete(qw(-p calculator-test)), q[delete project calculator-test]);
ok(! -d "calculator-test", q[project directory has been deleted]);

exit 0;
