#!/usr/bin/perl

use Test::More tests => 24;
use t::util;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
diag("using cached_attributes") if VCS::CMSynergy::use_cached_attributes();

my $e_got = $ccm->query_object("name match '*blurfl*'");
ok(UNIVERSAL::isa($e_got, "ARRAY") && @$e_got == 0,
   q[query with no match: $ccm->query_object("name match '*blurfl*'")]);

# test query_object with old-style objectnames returned from the query
my $b_expected;
my $b45_expected = [
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
];
my $b5_expected = [
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
];
my $b6_expected =
[
    @$b5_expected,
   'bitmap-1:attype:base',
   'bitmap-1:cvtype:base',
];
my $b63_expected =
[
    @$b6_expected,
    'baseline-1:cvtype:base',
];
for (scalar $ccm->version)
{
    /^4\.5/			&& do { $b_expected = $b45_expected; last };
    /^5\./			&& do { $b_expected = $b5_expected; last };
    /^6\.(\d+)/ && $1 < 3	&& do { $b_expected = $b6_expected; last; };
    /^6\.(\d+)/			&& do { $b_expected = $b63_expected; last; };
    die "don't know anything about CM Synergy version $_";
}
my $b_got = $ccm->query_object("name match 'b*'");
verbose('b_got', $b_got);
isa_ok($b_got, "ARRAY", q[query_object()]);
all_ok { UNIVERSAL::isa($_, "VCS::CMSynergy::Object") } $b_got,
   q[query_object() returns array ref of V::C::Os];
ok(eq_set($b_expected, objectnames($b_got)),
   q[$ccm->query_object("name match 'b*'")]);

# test query_arrayref with a multi-line valued keyword
my $ml_expected = 
[
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
isa_ok($ml_got, "ARRAY", q[query_arrayref()]);
all_ok { UNIVERSAL::isa($_, "ARRAY") } $ml_got,
   q[query_arrayref() returns array ref of array ref];
ok(eq_array(		# eq_set does not properly cope with refs
   [ sort { $a->[0] cmp $b->[0] } @$ml_got ], 
   [ sort { $a->[0] cmp $b->[0] } @$ml_expected ]),
   q[query for multi-line attributes: $ccm->query_arrayref("name = 'bufcolor.c' and instance = '1'", qw(objectname status_log))]); 

# test shorthand queries
my $sh1_got = $ccm->query_object({ name => "bufcolor.c", instance => 1 });
verbose('sh1_got', $sh1_got);
isa_ok($sh1_got, "ARRAY", q[query_object()]);
ok(eq_set([ map { $_->[0] } @$ml_expected ], objectnames($sh1_got)),
   q[shorthand query with several clauses]);

my $sh2_expected = 
[
  'calculator-1.0:project:1',
  'editor-1.0:project:1',
  'guilib-1.0:project:1',
  'toolkit-1.0:project:1',
];
my $sh2_got = $ccm->query_object({ hierarchy_project_members => [ 'toolkit-1.0:project:1', 'none' ] });
verbose('sh2_got', $sh2_got);
isa_ok($sh2_got, "ARRAY", q[query_object()]);
ok(eq_set($sh2_expected, objectnames($sh2_got)),
   q[shorthand query with hierarchy_project_members()]);

# task6: Add some fonts to the GUI library for use in the editor
my $sh3_expected =
[
  'fonts.c-1:csrc:1',
  'fonts.h-1:incl:1',
  'includes-2:dir:3',
  'main.c-2:csrc:2',
  'makefile-2:makefile:2',
  'makefile-2:makefile:3',
  'makefile.pc-2:makefile:2',
  'makefile.pc-2:makefile:3',
  'readme-2:ascii:1',
  'sources-2:dir:2',
];
my $sh3_got = $ccm->query_object({ task => 6 });
verbose('sh3_got', $sh3_got);
isa_ok($sh3_got, "ARRAY", q[query_object()]);
ok(eq_set($sh3_expected, objectnames($sh3_got)),
   q[shorthand query with task]);

my $complex_expected = 
[
  [ 'task-1:cvtype:base',	'base/cvtype/task/1' ],
  [ 'task1-1:task:probtrac',	'1' ],
  [ 'task25-1:task:probtrac',	'25' ],
  [ 'task26-1:task:probtrac',	'26' ],
  [ 'task27-1:task:probtrac',	'27' ],
  [ 'task28-1:task:probtrac',	'28' ],
  [ 'task6-1:task:probtrac',	'6' ],
  [ 'text-1:attype:AC',		'AC/attype/text/1' ],
  [ 'time-1:attype:AC',		'AC/attype/time/1' ],
  [ 'toolkit-1:dir:1',		'toolkit-1' ],
  [ 'toolkit-1.0:project:1',	'toolkit-1.0' ],
  [ 'toolkit-darcy:project:1',	'toolkit-darcy' ],
  [ 'toolkit-int:project:1',	'toolkit-int' ],
  [ 'toolkit.ini-1:ascii:1',	'toolkit.ini-1' ],
  [ 'tset-1:cvtype:base',	'base/cvtype/tset/1' ]
];
push @$complex_expected,
  [ 'toolkit-int_20021125:project:1', 'toolkit-int_20021125' ]
  if $ccm->version >= 6.3;

# NOTE: Exclude automatic tasks as they are unpredictable;
# exlude baselines, releasedefs, and recon_temps (CM Synergy >= 6.3 only).
my $complex_got = $ccm->query_arrayref(
    "name match 't*' and not ( (cvtype = 'task' and status = 'task_automatic') or cvtype = 'baseline' or cvtype = 'recon_temp' or cvtype = 'releasedef' )", 
    qw(objectname displayname));
verbose('complex_got', $complex_got);
isa_ok($complex_got, "ARRAY", q[query_arrayref() returns array ref]);
all_ok { UNIVERSAL::isa($_, "ARRAY") } $complex_got, 
   q[query_arrayref() returns array ref of array refs];
ok(eq_array(			# sort by first array element (object, i.e. objectname)
   [ sort { $a->[0] cmp $b->[0] } @$complex_expected ],
   [ sort { $a->[0] cmp $b->[0] } @$complex_got ]),
   q[query "name match 't*'"]);
all_ok { $ccm->property(displayname => $_->[0]) eq $_->[1] } $complex_got,
   q[check for property(displayname)];

# test query with attribute caching
my $bc_got = $ccm->query_object_with_attributes(
    "name match 'b*'", qw(create_time owner status_log super_type));
verbose('bc_got', $bc_got);
isa_ok($bc_got, "ARRAY", q[query_object_with_attributes()]);
all_ok { UNIVERSAL::isa($_, "VCS::CMSynergy::Object") } $bc_got,
   q[query_object_with_attributes() returns array ref of V::C::Os];
ok(eq_set($b_expected, objectnames($bc_got)),
   q[$ccm->query_object("name match 'b*'")]);
# check for actually cached attributes (type specific)
all_ok {
    my $acache = $_->_private->{acache};
    my $list = $_->list_attributes;
    foreach my $attr (qw(create_time owner status_log super_type))
    {
	return 0 if defined($list->{$attr}) xor defined($acache->{$attr});
    }
    return 1;
} $bc_got,
   qq[returned objects have cached attributes];

exit 0;
