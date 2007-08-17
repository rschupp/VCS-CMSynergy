#!/usr/bin/perl

use Test::More tests => 8;
use t::util;
use UNIVERSAL qw(isa);

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $e_got = $ccm->query_object("name match '*blurfl*'");
ok(isa($e_got, "ARRAY") && @$e_got == 0,
   q[$ccm->query_object("name match '*blurfl*'") -- no match]);

# test query_object with old-style objectnames returned from the query
my $b_expected;
for (scalar $ccm->version)
{
    /^4\.5/ && do { $b_expected = [
       'base-1:admin:base',
       'base-1:mcomp:base',
       'base-1:model:base',
       'binary-1:attype:base',
       'binary-1:cvtype:base',
       'binary-1:pdtype:base',
       'boolean-1:attype:AC',
       'bstype-1:cvtype:AC',
       'bstypes-1:mcomp:AC',
       'bstypes-1:mcomp:base',
       'bufcolor.c-1:csrc:1',
       'bufcolor.c-1:csrc:2',
       'bufcolor.c-2:csrc:1',
       'bufcolor.c-2:csrc:2',
       'bufcolor.c-3:csrc:1',
       'bufcolor.c-3:csrc:2',
       'by_directory-1:bstype:base',
       'by_name-1:bstype:AC',
       'by_name0-1:bstype:base',
       'by_name_wa-1:bstype:base',
    ], last };

    /^5\./ && do { $b_expected = [
       'base-1:admin:base',
       'base-1:mcomp:base',
       'base-1:model:base',
       'binary-1:attype:base',
       'binary-1:cvtype:base',
       'boolean-1:attype:AC',
       'bstype-1:cvtype:AC',
       'bstypes-1:mcomp:AC',
       'bstypes-1:mcomp:base',
       'bufcolor.c-1:csrc:1',
       'bufcolor.c-1:csrc:2',
       'bufcolor.c-2:csrc:1',
       'bufcolor.c-2:csrc:2',
       'bufcolor.c-3:csrc:1',
       'bufcolor.c-3:csrc:2',
       'by_directory-1:bstype:base',
       'by_name-1:bstype:AC',
    ], last };

    /^6\./ && do { $b_expected = [
       'base-1:admin:base',
       'base-1:mcomp:base',
       'base-1:model:base',
       'binary-1:attype:base',
       'binary-1:cvtype:base',
       'bitmap-1:attype:base',
       'bitmap-1:cvtype:base',
       'boolean-1:attype:AC',
       'bstype-1:cvtype:AC',
       'bstypes-1:mcomp:AC',
       'bstypes-1:mcomp:base',
       'bufcolor.c-1:csrc:1',
       'bufcolor.c-1:csrc:2',
       'bufcolor.c-2:csrc:1',
       'bufcolor.c-2:csrc:2',
       'bufcolor.c-3:csrc:1',
       'bufcolor.c-3:csrc:2',
       'by_directory-1:bstype:base',
       'by_name-1:bstype:AC',
    ], last };

    die "don't know anything about CM Synergy version $_";
}
my $b_got = $ccm->query_object("name match 'b*'");
verbose('b_got', $b_got);
isa_ok($b_got, "ARRAY", "query_object() returns array ref");
ok(all(sub { defined $_  && isa($_, "VCS::CMSynergy::Object") }, @$b_got),
   q[query_object() returns array ref of VCS::CMSynergy::Objects]);
ok(eq_set($b_expected, [ map { "$_" } @$b_got ]),
   q[$ccm->query_object("name match 'b*'")]);

# test query_arrayref with a multi-line valued keyword
my $ml_expected = [
   [
     'bufcolor.c-1:csrc:1',
     'Tue Jun 25 09:47:34 1996: Status set to \'working\' by joe in role developer
Tue Jun 25 11:41:05 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 15:29:14 1996: Status set to \'test\' by joe in role ccm_admin
Wed Sep 24 08:58:21 1997: Status set to \'released\' by darcy in role ccm_admin
'
   ],
   [
     'bufcolor.c-2:csrc:1',
     'Tue Jun 25 11:41:50 1996: Status set to \'working\' by joe in role build_mgr
Tue Jun 25 11:42:07 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 15:29:16 1996: Status set to \'test\' by joe in role ccm_admin
Mon Sep 29 17:55:46 1997: Status set to \'integrate\' by darcy in role ccm_admin
'
   ],
   [
     'bufcolor.c-3:csrc:1',
     'Tue Jun 25 11:43:50 1996: Status set to \'working\' by joe in role build_mgr
Tue Jun 25 11:44:22 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 11:54:22 1996: Status set to \'test\' by joe in role build_mgr
Mon Sep 29 17:56:01 1997: Status set to \'integrate\' by darcy in role ccm_admin
'
   ]
];
my $ml_got = $ccm->query_arrayref(
    "name = 'bufcolor.c' and instance = '1'", qw(objectname status_log));
verbose('ml_got', $ml_got);
isa_ok($ml_got, "ARRAY", "query_arrayref() returns array ref");
ok(all(sub { isa($_, "ARRAY") }, @$ml_got),
   q[query_arrayref() returns array ref of array ref]);
ok(eq_array(			# sort by first array element (objectname)
   [ sort { $a->[0] cmp $b->[0] } @$ml_expected ],
   [ sort { $a->[0] cmp $b->[0] } @$ml_expected ]),
   q[$ccm->query_arrayref("name = 'bufcolor.c' and instance = '1'", qw(objectname status_log))]); 

exit 0;
