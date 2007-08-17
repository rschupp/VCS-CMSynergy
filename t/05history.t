#!/usr/bin/perl

use Test;
BEGIN { plan tests => 8 }
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session);
ok(ref $ccm, 'VCS::CMSynergy');
print "# using Expect\n" if $ccm->{exp};

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
my $h0_result = $ccm->history("bufcolor.c-1:csrc:2");
verbose('h0_result', $h0_result);
ok(ref $h0_result, 'ARRAY');
# sigh, CCM_DATETIME_FMT doesn't work with Windows clients
if ($^O eq 'MSWin32' || $^O eq 'cygwin')
{
    s/^Created:.*$/Created:/m foreach (@$h0_exp);
    s/^Created:.*$/Created:/m foreach (@$h0_result);
}
ok(eqarray($h0_exp, $h0_result));

my $h1_exp = [
   {
     'object' => 'bufcolor.c-1:csrc:2',
     'task' => undef,
     'status_log' => 'Fri Sep  5 08:38:16 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 08:53:24 1997: Status set to \'released\' by ccm_root in role ccm_admin
',
     'predecessors' => [],
     'successors' => [ 'bufcolor.c-2:csrc:2' ]
   },
   {
     'object' => 'bufcolor.c-2:csrc:2',
     'task' => '25',
     'status_log' => 'Fri Sep  5 09:04:57 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 09:06:02 1997: Status set to \'integrate\' by ccm_root in role ccm_admin
',
     'predecessors' => [ 'bufcolor.c-1:csrc:2' ],
     'successors' => [ 'bufcolor.c-3:csrc:2' ]
   },
   {
     'object' => 'bufcolor.c-3:csrc:2',
     'task' => '26',
     'status_log' => 'Fri Sep  5 09:06:20 1997: Status set to \'working\' by ccm_root in role ccm_admin
Fri Sep  5 09:06:48 1997: Status set to \'integrate\' by ccm_root in role ccm_admin
',
     'predecessors' => [ 'bufcolor.c-2:csrc:2' ],
     'successors' => []
   }
];
my $h1_result = $ccm->history_hashref(
    "bufcolor.c-3:csrc:2", qw(object predecessors successors task status_log));
verbose('h1_result', $h1_result);
ok(ref $h1_result, 'ARRAY');
ok(all { ref $_ eq 'HASH' } @$h1_result);
ok(all { ref $_->{object} eq "VCS::CMSynergy::Object" } @$h1_result);
ok(all 
    { 
	all { ref $_ eq "VCS::CMSynergy::Object" } 
	    @{ $_->{predecessors} }, @{ $_->{successors} } 
    } @$h1_result);
# stringisize Objects in $h1_result for following comparison
foreach my $row (@$h1_result)
{
    $row->{object} = "$row->{object}";
    $_ = "$_" foreach (@{ $row->{predecessors} }); 
    $_ = "$_" foreach (@{ $row->{successors} }); 
}
ok(eqarray(
    $h1_exp, $h1_result,
    sub { $a->{object} cmp $b->{object} } # sort on key object
    ));
exit 0;
