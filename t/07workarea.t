#!/usr/bin/perl -w

use Test::More tests => 26;
use t::util;
use Cwd;
use File::Path;
use File::Spec;

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	require Filesys::CygwinPaths; import Filesys::CygwinPaths qw(:all);
    }
}

my @cleanup;			# cleanup actions

END
{
    &{ pop @cleanup } while @cleanup;
}


my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# make sure there are no remnants from previous tests
rmtree("calculator-test", 0, 0);

my $pwd = getcwd();
$pwd = fullwin32path($pwd) if $^O eq 'cygwin';


ok($ccm->checkout(qw/-project calculator-1.0 -copy_based -to test/, -path => $pwd),
   q[checkout project calculator-1.0 to calculator-test]);
push @cleanup, sub
{
    ok($ccm->delete(qw(-project calculator-test)), 
	q[delete project calculator-test]);
    ok(! -d "calculator-test", 
	q[project directory has been deleted]);
};

my $ccmwaid = File::Spec->catfile(
    qw(calculator-test calculator),
    $VCS::CMSynergy::Is_MSWin32 ? "_ccmwaid.inf" : ".ccmwaid.inf");
ok(-e $ccmwaid, qq[check for ccmwaid file ($ccmwaid)]);

# chdir to project sub directory (esp. for testing coprocess)
ok(chdir(File::Spec->catdir(qw(calculator-test calculator sources))), 
   q[chdir to project sub directory (sources)]);
push @cleanup, sub { chdir($pwd) };

my $file = "clear.c";

ok(-e $file, qq[file $file exists]);
ok(! -w $file, qq[file $file is read-only]);

# use workarea name to specify an object
is($ccm->get_attribute(status => $file), "released",
   q[get_attribute via workarea name]);

# check out an object
ok($ccm->checkout($file), q[check out $file]);
push @cleanup, sub
{
    ok($ccm->delete(-replace => $file), qq[delete and replace $file]);
};
ok(-w $file, qq[file $file is writable]);
is($ccm->get_attribute(status => $file), "working", 
    q[checked out file is "working"]);

# test set_attribute with different values
my $value;

$value = "the quick brown fox jumps over the lazy dog";
ok($ccm->set_attribute(comment => $file, $value),
    q[set_attribute to simple string]);
is($ccm->get_attribute(comment => $file), $value,
    q[re-get_attribute and compare]);

$value = join("-" x 10, 1..100);
ok($ccm->set_attribute(comment => $file, $value),
    q[set_attribute to long string]);
is($ccm->get_attribute(comment => $file), $value,
    q[re-get_attribute and compare]);

$value = join("\n", 1..10);
ok($ccm->set_attribute(comment => $file, $value),
    q[set_attribute to string with newlines]);
is($ccm->get_attribute(comment => $file), $value,
    q[re-get_attribute and compare]);

$value = join(" ", map { qq["$_"] } 1..10);
ok($ccm->set_attribute(comment => $file, $value),
    q[set_attribute to string with embedded quotes]);
is($ccm->get_attribute(comment => $file), $value,
    q[re-get_attribute and compare]);

my ($object) = @{ $ccm->ls_object($file) };
isa_ok($object, "VCS::CMSynergy::Object");

my $displayname = $ccm->property(displayname => $file);
is($displayname, $object->name . $ccm->delimiter . $object->version,
    q[check for expected displayname]);

SKIP: 
{
    skip "no tied hash interface to VCS::CMSynergy::Object", 3 
	unless tied %$object;

    is($object->{status}, "working", q[FETCH attribute]);
    $value = "set via tied hash interface";
    $object->{comment} = $value;
    is($object->{comment}, $value, q[re-FETCH attribute and compare]);
    is($object->displayname, $displayname, q[check displayname()]);
}

exit 0;

