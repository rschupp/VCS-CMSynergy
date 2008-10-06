#!/usr/bin/perl

use Test::More tests => 19;
use t::util;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}

use File::Temp qw(tempfile);

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# test tracing
{
    my ($fh, $trace) = tempfile;
    close($fh);

    $ccm->trace(1, $trace);
    $ccm->trace_msg(q[the quick brown fox jumps over the lazy dog]);
    $ccm->trace(0, undef);
    ok(-r $trace, q[trace file exists]);

    my $text;
    {
	local $/ = undef; 
	ok(open($fh, "< $trace"), q[open trace file]);
	$text = <$fh>;
	close($fh);
    }
    ok($text =~ /quick brown fox/, q[trace file contains message]);
    ok($text =~ /\Q$ccm\E/, q[trace file contains session ref]);

    unlink($trace);
}

# test object<->cvid
my $object = $ccm->object('main.c-1:csrc:1');
ok($object->exists, qq[object exists]);
my $cvid = $ccm->property(cvid => $object);
like($cvid, qr/^\d+$/, q[check cvid]);
my $object_from_cvid = $ccm->object_from_cvid($cvid);
is("$object", "$object_from_cvid", q[object -> cvid -> same_object]);

# test per object application data
my %blurfl = ( foo => 1, bar => 2, quux => 3 );
$object->mydata->{$_} = $blurfl{$_} foreach keys %blurfl;
cmp_deeply($object->mydata, \%blurfl, qq[object mydata]);

# test that the same objectname gives identical Perl objects
# (when use_cached_attributes is in effect):
# we need to rebless the VCS::CMSynergy::Objects into a dummy class
# in order to compare them, otherwise the overloaded stringification
# gets in our way
SKIP: 
{
    skip "not using :cached_attributes", 1
	unless VCS::CMSynergy::use_cached_attributes();

    my $object2 = $ccm->object($object->objectname);
    is(bless($object, "Reblessed"), bless($object2, "Reblessed"), 
	q[identical Perl objects from same objectname when using :cached_attributes]);
}

# test that exists() on an non-existant object doesn't cause an exception 
ok(!$ccm->object('frobozz.c-1:csrc:1')->exists, q[object doesn't exist]);  #'

SKIP: 
{
    skip "get_releases() is obsolete in CM Synergy >= 6.3", 2
	unless $ccm->version <= 6.2;

    my $rel_expected = {
	'1.0'	=> [ qw(1.0) ],
	'1.1'	=> [ qw(1.0 1.1) ],
	'2.0'	=> [ qw(1.0 1.1 2.0) ],
	'2.0_SP1'	=> [ qw(1.0 1.1 2.0 2.0_SP1) ],
	'2.1'	=> [ qw(1.0 1.1 2.0 2.1) ],
	'3.0'	=> [ qw(1.0 1.1 2.0 2.1 3.0) ],
	'3.1'	=> [ qw(1.0 1.1 2.0 2.1 3.0 3.1) ]
    };
    my $rel_got = $ccm->get_releases;
    isa_ok($rel_got, "HASH", q[get_releases()]);
    ok(eq_hash($rel_got, $rel_expected, q[$ccm->get_releases]),
	q[check release table]);
}


BEGIN { use_ok('VCS::CMSynergy::Users'); }

my $users = $ccm->users;
isa_ok($users, 'HASH', q[users()]);
cmp_deeply([ values %$users ], array_each(isa('ARRAY')), 
  q[users(): isa HASH of ARRAY]);
ok(exists $users->{ccm_root}, q[ccm_root is in users]);

	
exit 0;
