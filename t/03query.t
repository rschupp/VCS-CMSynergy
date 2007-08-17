#!/usr/bin/perl

use Test::More tests => 8;
use t::util;
use UNIVERSAL qw(isa);

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using Expect") if defined $ccm->{exp};

my $e_result = $ccm->query_object("name match '*blurfl*'");
ok(isa($e_result, "ARRAY") && @$e_result == 0,
   q[$ccm->query_object("name match '*blurfl*'") -- no match]);
# test query_object with old-style objectnames returned from the query
# NOTE: exclude "bitmap" objects that appeared in CM Synergy 6.0
my $b_exp = [
   'base-1:admin:base',
   'base-1:mcomp:base',
   'base-1:model:base',
   'binary-1:attype:base',
   'binary-1:cvtype:base',
#  'bitmap-1:attype:base',	6.x only
#  'bitmap-1:cvtype:base',	6.x only
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
];
my $b_result = $ccm->query_object("name match 'b*' and name != 'bitmap'");
verbose('b_result', $b_result);
isa_ok($b_result, "ARRAY", "query_object() returns array ref");
ok(all(sub { defined $_  && isa($_, "VCS::CMSynergy::Object") }, @$b_result),
   q[query_object() returns array ref of VCS::CMSynergy::Objects]);
ok(eq_set($b_exp, [ map { "$_" } @$b_result ]),
   q[$ccm->query_object("name match 'b*' and name != 'bitmap'")]);

# test query_arrayref with a multi-line valued keyword
my $ml_exp = [
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
my $ml_result = $ccm->query_arrayref(
    "name = 'bufcolor.c' and instance = '1'", qw(objectname status_log));
verbose('ml_result', $ml_result);
isa_ok($ml_result, "ARRAY", "query_arrayref() returns array ref");
ok(all(sub { isa($_, "ARRAY") }, @$ml_result),
   q[query_arrayref() returns array ref of array ref]);
ok(eq_array(			# sort by first array element (objectname)
   [ sort { $a->[0] cmp $b->[0] } @$ml_exp ],
   [ sort { $a->[0] cmp $b->[0] } @$ml_exp ]),
   q[$ccm->query_arrayref("name = 'bufcolor.c' and instance = '1'", qw(objectname status_log))]); 

exit 0;
