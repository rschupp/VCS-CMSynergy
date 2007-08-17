#!/usr/bin/perl -w

use Test::More tests => 33;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

use Cwd;
use File::Path;
use File::Spec;
use File::Temp qw(tempdir tempfile);
use Digest::MD5;

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
    my $cleanup = Cleanup->new;

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
    $cleanup->add(sub
    {
	ok($ccm->delete(-project => $test_proj), 
	    qq[delete test project $test_proj]);
	ok(! -d $wa_path, 
	    q[test project workarea has been deleted]);
    });

    my $ccmwaid = File::Spec->catfile(
	$wa_path, $pname,
	VCS::CMSynergy::Client::is_win32 ? "_ccmwaid.inf" : ".ccmwaid.inf");
    ok(-e $ccmwaid, 
	qq[check for ccmwaid file ($ccmwaid) in workarea]);

    # chdir to workarea (for testing coprocess)
    {
	my $pwd = getcwd;
	ok(chdir($wa_path), q[chdir to workarea]);
	$cleanup->add(sub { chdir($pwd); });
    }

    # note MD5 of some objects for later test
    foreach my $obj ($ascii_obj, $binary_obj)
    {
	$result = $ccm->finduse($obj);
	ok(@$result == 1, qq[finduse returns one record]);
	my $fu = $result->[0]->[1];
	isa_ok($fu, "HASH", qq[finduse record cdr]);
	my $file = $path{$obj} = $fu->{$test_proj};
	ok(defined $file, qq[$obj found in project $test_proj]);
	ok(-e $file, qq[file ($file) for $obj found in workarea]);

	open my $fh, "<$file" or die "can't open $file: $!";
	binmode $fh;
	$md5->addfile($fh);
	close $fh;
	$md5_expected{$obj} = $md5->hexdigest;
    }

    my $file = $path{$ascii_obj};
    ok(-e $file, qq[file $file exists]);
    ok(! -w $file, qq[file $file is read-only]);

    # use workarea name to specify an object
    is($ccm->get_attribute(status => $file), "released",
       q[get_attribute via workarea name]);

    # check out an object
    ok($ccm->checkout($file), q[check out $file]);
    $cleanup->add(sub
    {
	ok($ccm->delete(-replace => $file), 
	    qq[delete and replace $file]);
    });
    ok(-w $file, qq[file $file is now writable]);
    is($ccm->get_attribute(status => $file), "working", 
	q[checked out file is "working"]);
}

foreach my $obj ($ascii_obj, $binary_obj)
{
    my (undef, $tmpfile) = tempfile(CLEANUP => 1);
    ok($ccm->cat_object($obj, $tmpfile), q[cat_object to file]);

    open my $fh, "<$tmpfile" or die "can't open $tmpfile: $!";
    binmode $fh;
    $md5->addfile($fh);
    close $fh;
    is($md5->hexdigest, $md5_expected{$obj}, qq[compare MD5 for $obj]);

    my $contents;
    ok($ccm->cat_object($obj, \$contents), q[cat_object to string]);
    $md5->add($contents);
    is($md5->hexdigest, $md5_expected{$obj}, qq[compare MD5 for $obj]);
}
    
exit 0;

package Cleanup;

sub new		{ my $self = bless [], shift; $self->add(@_); $self }
sub add		{ my $self = shift; push @$self, @_; $self; }
sub DESTROY	{ my $self = shift; &{ pop @$self } while @$self; }
