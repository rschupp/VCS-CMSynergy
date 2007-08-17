#!/usr/bin/perl -w

use Test::More tests => 45;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

use Cwd;
use File::Path;
use File::Spec;
use File::Temp qw(tempdir tempfile);
use Digest::MD5;
use End;

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	require Filesys::CygwinPaths; 
	import Filesys::CygwinPaths qw(:all);
    }
}


my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $pname = "calculator";
my $pversion = "test$$";

my $result = $ccm->query_object(
    qq[type='project' and name='$pname' and version match 'test*']);
ok(@$result == 0, qq[test project ${pname}-test* does not exist yet]);

my $md5 = Digest::MD5->new;
my (%md5_expected, %path);
my $ascii_obj = $ccm->object("clear.c-1:csrc:2");
my $binary_obj = $ccm->object("calculator.exe-1:executable:1");

{
    my $tempdir = tempdir(CLEANUP => 1);
    $tempdir = fullwin32path($tempdir) if $^O eq 'cygwin';

    ok($ccm->checkout(-project => "${pname}-1.0", 
		      -to => $pversion, -path => $tempdir, "-copy_based"), 
	qq[checkout project ${pname}-1.0 to $pversion]);
    $result = $ccm->query_object({ type => 'project', name => $pname, version => $pversion });
    ok(@$result == 1, 
	qq[test project ${pname}-${pversion} has been created]);
    my $test_proj = $result->[0];
    my $wa_path = $ccm->get_attribute(wa_path => $test_proj);
    ok(index($wa_path, $tempdir) == 0, 
	qq[wa_path "$wa_path" is below checkout path "$tempdir"]);
    my $cleanup_test_proj = end
    {
	ok($ccm->delete(-project => $test_proj), 
	    qq[delete test project $test_proj]);
	ok(! -d $wa_path, 
	    q[test project workarea has been deleted]);
    };

    my $ccmwaid = File::Spec->catfile(
	$wa_path, $pname,
	VCS::CMSynergy::Client::is_win32 ? "_ccmwaid.inf" : ".ccmwaid.inf");
    ok(-e $ccmwaid, 
	qq[check for ccmwaid file ($ccmwaid) in workarea]);

    # chdir to workarea (for testing coprocess)
    my $pwd = getcwd;
    ok(chdir($wa_path), q[chdir to workarea]);
    my $cleanup_chdir = end { chdir($pwd); };

    foreach my $obj ($ascii_obj, $binary_obj)
    {
	$result = $ccm->finduse($obj);
	ok(@$result == 1, qq[finduse returns one record]);
	my $fu = $result->[0]->[1];
	isa_ok($fu, "HASH", qq[finduse record cdr]);
	my $file = $path{$obj} = $fu->{$test_proj};
	ok(defined $file, qq[$obj found in project $test_proj]);
	ok(-e $file, qq[file ($file) for $obj found in workarea]);

	# note MD5 for later tests
	$md5_expected{$obj} = md5_file($file);
    }

    my $file = $path{$ascii_obj};
    ok(-e $file, qq[file $file exists]);
    ok(! -w $file, qq[file $file is read-only]);

    # use workarea name to specify an object
    is($ccm->get_attribute(status => $file), "released",
       q[get_attribute via workarea name]);

    # check out an object
    ok($ccm->checkout($file), q[check out $file]);
    my $cleanup_checkout = end 
    {
	ok($ccm->delete(-replace => $file), 
	    qq[delete and replace $file]);
    };
    ok(-w $file, qq[file $file is now writable]);
    is($ccm->get_attribute(status => $file), "working", 
	q[checked out file is "working"]);
}

foreach my $obj ($ascii_obj, $binary_obj)
{
    my (undef, $tmpfile) = tempfile(CLEANUP => 1);
    ok($ccm->cat_object($obj, $tmpfile), q[cat_object to file]);
    is(md5_file($tmpfile), $md5_expected{$obj}, qq[compare MD5 for $obj]);

    my $contents;
    ok($ccm->cat_object($obj, \$contents), q[cat_object to string]);
    is($md5->add($contents)->hexdigest, $md5_expected{$obj}, qq[compare MD5 for $obj]);

    my $retval = $ccm->cat_object($obj);
    ok(defined $retval, q[cat_object return contents]);
    is($md5->add($retval)->hexdigest, $md5_expected{$obj}, qq[compare MD5 for $obj]);

    my ($fh, $tmpfile2) = tempfile(CLEANUP => 1);
    ok($ccm->cat_object($obj, $fh), q[cat_object to filehandle]);
    close $fh;
    is(md5_file($tmpfile2), $md5_expected{$obj}, qq[compare MD5 for $obj]);
}

# test with CODE and ARRAY for ascii object only 
# (because they operate by lines)

my @lines;
ok($ccm->cat_object($ascii_obj, \@lines), q[cat_object to array]);
$md5->add($_) foreach @lines;
is($md5->hexdigest, $md5_expected{$ascii_obj}, qq[compare MD5 for $ascii_obj]);

ok($ccm->cat_object($ascii_obj, sub { $md5->add($_); }), q[cat_object to sub]);
is($md5->hexdigest, $md5_expected{$ascii_obj}, qq[compare MD5 for $ascii_obj]);

    
exit 0;

sub md5_file
{
    my ($file) = @_;
    open my $fh, "<$file" or die "can't open $file: $!";
    binmode $fh;
    $md5->addfile($fh);
    close $fh;
    return $md5->hexdigest;
}
