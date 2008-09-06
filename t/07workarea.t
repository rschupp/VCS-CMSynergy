#!/usr/bin/perl -w

use Test::More tests => 55;
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

my %md5_expected;

my %objects =
(
    "clear.c-1:csrc:2"			=> "calculator/sources/clear.c",
    "calculator.exe-1:executable:1"	=> "calculator/calculator.exe",
);


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

    # set Object_format so that we may predict the first element
    # of an element of @$finduse
    my $old_format = $ccm->set(Object_format => "%objectname");
    my $finduse = $ccm->finduse(keys %objects);
    $ccm->set(Object_format => $old_format);
    isa_ok($result, "ARRAY", qq[finduse return value]);

    foreach my $name (keys %objects)
    {
	my ($info, $uses) = @{ shift @$finduse };
	is($info, $name, qq[finduse elem[0] for $name]);
	isa_ok($uses, "HASH", qq[finduse elem[1] for $name]);

	my $path = $uses->{$test_proj};
	is($path, $objects{$name}, qq[$name found in project $test_proj]);
	ok(-e $path, qq[$name found in workarea as $path]);

	# note MD5 for later tests
	$md5_expected{$name} = md5_file($path);

	# use workarea name to specify an object
	is($ccm->get_attribute(status => $path), "released",
	   q[get_attribute via workarea name]);

	# check out an object
	ok(! -w $path, qq[file $path is read-only]);
	ok($ccm->checkout($path), q[check out $path]);
	my $cleanup_checkout = end 
	{
	    ok($ccm->delete(-replace => $path), 
		qq[delete and replace $path]);
	};
	ok(-w $path, qq[file $path is now writable]);
	is($ccm->get_attribute(status => $path), "working", 
	    q[checked out file is "working"]);
    }
}

# test cat_object()
my $md5 = Digest::MD5->new;
foreach my $name (keys %objects)
{
    my $obj = $ccm->object($name);
    my $md5_exp = $md5_expected{$name};

    my (undef, $tmpfile) = tempfile(CLEANUP => 1);
    ok($ccm->cat_object($obj, $tmpfile), q[cat_object to file]);
    is(md5_file($tmpfile), $md5_exp, qq[compare MD5 for $obj]);

    my $contents;
    $md5->reset;
    ok($ccm->cat_object($obj, \$contents), q[cat_object to SCALAR]);
    is($md5->add($contents)->hexdigest, $md5_exp, qq[compare MD5 for $obj]);

    $md5->reset;
    my $retval = $ccm->cat_object($obj);
    ok(defined $retval, q[cat_object returns contents]);
    is($md5->add($retval)->hexdigest, $md5_exp, qq[compare MD5 for $obj]);

    my ($fh, $tmpfile2) = tempfile(CLEANUP => 1);
    ok($ccm->cat_object($obj, $fh), q[cat_object to filehandle]);
    close $fh;
    is(md5_file($tmpfile2), $md5_exp, qq[compare MD5 for $obj]);

    # test second argument of CODE or ARRAY for ascii objects only 
    # (because these operate by lines)
    SKIP: 
    {
	skip "cat_object to CODE or ARRAY on non-ascii object", 4
	    unless $obj->get_attribute("super_type") eq "ascii";

	$md5->reset;
	my @lines;
	ok($ccm->cat_object($obj, \@lines), q[cat_object to ARRAY]);
	$md5->add($_) foreach @lines;
	is($md5->hexdigest, $md5_exp, qq[compare MD5 for $obj]);

	$md5->reset;
	ok($ccm->cat_object($obj, sub { $md5->add($_); }), q[cat_object to CODE]);
	is($md5->hexdigest, $md5_exp, qq[compare MD5 for $obj]);
    }
}

    
exit 0;

sub md5_file
{
    my ($file) = @_;
    my $md5 = Digest::MD5->new;
    open my $fh, "<", $file or die "can't open $file: $!";
    binmode $fh;
    $md5->addfile($fh);
    close $fh;
    return $md5->hexdigest;
}
