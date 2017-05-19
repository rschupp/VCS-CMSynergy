#!/usr/bin/perl

use Test::More tests => 13;
use lib '.';
use xt::util;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}


my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

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


BEGIN { use_ok('VCS::CMSynergy::Users'); }

my $users = $ccm->users;
isa_ok($users, 'HASH', q[users()]);
cmp_deeply([ values %$users ], array_each(isa('ARRAY')), 
  q[users(): isa HASH of ARRAY]);
ok(exists $users->{ccm_root}, q[ccm_root is in users]);

	
exit 0;
