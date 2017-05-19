#!/usr/bin/perl

use Test::More tests => 10;
use lib '.';
use xt::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $h0_exp = [
'Object:  bufcolor.c-1 (csrc:2)
Owner:   jwark
State:   released
Created: 1997-09-05 17:38:16
Task:    <void>
Comment:
Baseline of source code migrated in for the sandbox project.

Predecessors:
Successors:
	bufcolor.c-2:csrc:2
',
'Object:  bufcolor.c-2 (csrc:2)
Owner:   connor
State:   integrate
Created: 1997-09-05 18:04:57
Task:    25
Comment:
Enhanced to support rgb settings
Predecessors:
	bufcolor.c-1:csrc:2
Successors:
	bufcolor.c-3:csrc:2
',
'Object:  bufcolor.c-3 (csrc:2)
Owner:   hannah
State:   integrate
Created: 1997-09-05 18:06:19
Task:    26
Comment:
Fixed problem with buffer handling
Predecessors:
	bufcolor.c-2:csrc:2
Successors:
'
];
my $h0_got = $ccm->history("bufcolor.c-1:csrc:2");
verbose('h0_got', $h0_got);
isa_ok($h0_got, "ARRAY", "history()");
# - datetimes can't be handled reliably (cf. README.datetime)
# - web mode omits "State:"
# - blank lines vary
s/^(Created:.*|State:.*|\s*)\n//gm foreach @$h0_exp, @$h0_got;
ok(eq_set($h0_got, $h0_exp),
   q[$ccm->history("bufcolor.c-1:csrc:2")]);

my $h1_exp = [
   {
     object => vco('bufcolor.c:1:csrc:2'),
     task => undef,
     status_log => 'Fri Sep  5 08:38:16 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 08:53:24 1997: Status set to \'released\' by ccm_root in role ccm_admin',
     predecessors => [],
     successors => vcoset([ qw(bufcolor.c:2:csrc:2) ]),
   },
   {
     object => vco('bufcolor.c:2:csrc:2'),
     task => '25',
     status_log => 'Fri Sep  5 09:04:57 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 09:06:02 1997: Status set to \'integrate\' by ccm_root in role ccm_admin',
     predecessors => vcoset([ qw(bufcolor.c:1:csrc:2) ]),
     successors => vcoset([ qw(bufcolor.c:3:csrc:2) ]),
   },
   {
     object => vco('bufcolor.c:3:csrc:2'),
     task => '26',
     status_log => 'Fri Sep  5 09:06:20 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 09:06:48 1997: Status set to \'integrate\' by ccm_root in role ccm_admin',
     predecessors => vcoset([ qw(bufcolor.c:2:csrc:2) ]),
     successors => [],
   }
];
my $h1_got = $ccm->history_hashref(
    "bufcolor.c-3:csrc:2", qw(object predecessors successors task status_log));
verbose('h1_got', $h1_got);
cmp_deeply($h1_got, array_each(isa("HASH")),
   q[history_hashref(): isa ARRAY of HASH]);
cmp_bag($h1_got, $h1_exp,
    q[history_hashref("bufcolor.c-3:csrc:2", ...)]);

# test autoloaded is_FOO_of/has_FOO V::C::O methods
my $predecessors = $ccm->object("bufcolor.c-3:csrc:2")->has_successor;
cmp_deeply($predecessors, array_each(isa("VCS::CMSynergy::Object")), 
    q[has_successor() returns list of V::C::Os]);
cmp_vcos($predecessors, [ qw(bufcolor.c:2:csrc:2) ], q[has_successor check]);
my $successors = $ccm->object("bufcolor.c-2:csrc:2")->is_successor_of;
cmp_deeply($successors, array_each(isa("VCS::CMSynergy::Object")),
    q[is_successor_of() returns list of V::C::Os]);
cmp_vcos($successors, [ qw(bufcolor.c:3:csrc:2) ], q[is_successor_of check]);

exit 0;
