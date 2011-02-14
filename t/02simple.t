#!/usr/bin/perl -w

use Test::More tests => 4;
use t::util;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}


my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");

$ccm->{RaiseError} = 0;

my $got = $ccm->query_object({ type => "project" }, qw( status ));
ok($got, "got any projects?");
my $ngot = @$got;
diag("your database contains $ngot project versions");

my %st;
$st{ $_->get_attribute("status") }++ foreach @$got;
diag("  $_: $st{$_}") foreach sort keys %st;

exit 0;

