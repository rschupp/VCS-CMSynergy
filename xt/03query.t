#!/usr/bin/perl -w

use Test::More tests => 24;
use xt::util;
use End;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}


my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
diag("using :cached_attributes") if VCS::CMSynergy::use_cached_attributes();

my $e_got = $ccm->query_object("name match '*blurfl*'");
cmp_vcos($e_got, [], q[query with no match returns empty array]);

# test that empty string values are correctly returned by query()
# (and not turned into undef)
{
    # we need a modifiable object...
    my $desc = gmtime()." the three stooges";
    my ($rc, $out, $err) = $ccm->folder(qw/-create -name/, $desc);
    ok($rc == 0, q[create folder]);
    my $rx_created = qr/Created folder (.*?)\./;
    like($out, $rx_created, "Created folder ...");
    my $folder = $ccm->folder_object($out =~ $rx_created);
    my $cleanup = end { $ccm->folder(qw/-delete -quiet -y/, $folder) };

    my @stooges = qw(larry moe curly);
    $ccm->attribute(-create => $_, -type => "string" , $folder) foreach @stooges;
    # NOTE: can't use create_attribute() with an empty string here,
    # because it doesn't work on Windows
    my ($attr) = @{ $ccm->query_hashref(
	[ type => "folder", description => $desc ], @stooges) };
    is($attr->{$_}, "", q[empty string attribute in query]) foreach @stooges;
}

# test query_object with old-style objectnames returned from the query
my @b_expected = qw(
    base-1:admin:base
    base-1:mcomp:base
    base-1:model:base
    binary-1:attype:base
    binary-1:cvtype:base
    boolean-1:attype:AC
    bstype-1:cvtype:AC
    bstypes-1:mcomp:AC
    bstypes-1:mcomp:base
    bufcolor.c-1:csrc:1
    bufcolor.c-1:csrc:2
    bufcolor.c-2:csrc:1
    bufcolor.c-2:csrc:2
    bufcolor.c-3:csrc:1
    bufcolor.c-3:csrc:2
    by_directory-1:bstype:base
    by_name-1:bstype:AC
);

push @b_expected, qw(
    bitmap-1:attype:base
    bitmap-1:cvtype:base)		if $ccm->version >= 6.0;
push @b_expected, qw(
    baseline-1:cvtype:base)		if $ccm->version >= 6.3;

my $b_query = "name match 'b*'";
my $b_got = $ccm->query_object($b_query);
verbose('b_got', $b_got);
cmp_vcos($b_got, \@b_expected, qq[query_object($b_query)]);
is($ccm->query_count($b_query), scalar @b_expected, q[query_count()]);

# test query_arrayref with a multi-line valued keyword
my $ml_expected = 
{
  'bufcolor.c-1:csrc:1' =>
  { 
     status_log => 'Tue Jun 25 09:47:34 1996: Status set to \'working\' by joe in role developer
Tue Jun 25 11:41:05 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 15:29:14 1996: Status set to \'test\' by joe in role ccm_admin
Wed Sep 24 08:58:21 1997: Status set to \'released\' by darcy in role ccm_admin'
   },
   'bufcolor.c-2:csrc:1' =>
   {
     status_log => 'Tue Jun 25 11:41:50 1996: Status set to \'working\' by joe in role build_mgr
Tue Jun 25 11:42:07 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 15:29:16 1996: Status set to \'test\' by joe in role ccm_admin
Mon Sep 29 17:55:46 1997: Status set to \'integrate\' by darcy in role ccm_admin'
   },
   'bufcolor.c-3:csrc:1' =>
   {
     status_log => 'Tue Jun 25 11:43:50 1996: Status set to \'working\' by joe in role build_mgr
Tue Jun 25 11:44:22 1996: Status set to \'integrate\' by joe in role build_mgr
Tue Jun 25 11:54:22 1996: Status set to \'test\' by joe in role build_mgr
Mon Sep 29 17:56:01 1997: Status set to \'integrate\' by darcy in role ccm_admin'
   }
};
my $ml_got = $ccm->query_object(
    "name = 'bufcolor.c' and instance = '1'", qw(status_log));
verbose('ml_got', $ml_got);
cmp_vcos($ml_got, $ml_expected, q[query for multi-line attributes]);

# test shorthand queries
my $sh1_got = $ccm->query_object([ name => "bufcolor.c", instance => 1 ]);
verbose('sh1_got', $sh1_got);
cmp_vcos($sh1_got, $ml_expected, q[shorthand query with several clauses]);

my @sh2_expected = qw(
   calculator-1.0:project:1
   editor-1.0:project:1
   guilib-1.0:project:1
   toolkit-1.0:project:1
);
my $sh2_got = $ccm->query_object(
    "hierarchy_project_members('toolkit-1.0:project:1', 'none'");
verbose('sh2_got', $sh2_got);
cmp_vcos($sh2_got, \@sh2_expected,
   q[query with hierarchy_project_members()]);

# task6: Add some fonts to the GUI library for use in the editor
my ($task6) = @{ $ccm->query_object(
    [
	type => "task",
	task_synopsis => "Add some fonts to the GUI library for use in the editor"
    ]) };
isa_ok($task6, "VCS::CMSynergy::Object", q[task 'Add some fonts...']);
verbose('task6', $task6);

my @sh3_expected = qw(
  fonts.c-1:csrc:1
  fonts.h-1:incl:1
  includes-2:dir:3
  main.c-2:csrc:2
  makefile-2:makefile:2
  makefile-2:makefile:3
  makefile.pc-2:makefile:2
  makefile.pc-2:makefile:3
  readme-2:ascii:1
  sources-2:dir:2
);
my $sh3_got = $ccm->query_hashref(
    "is_associated_cv_of('$task6')", qw(object task_objects));
verbose('sh3_got', $sh3_got);
cmp_vcos([ map { $_->{object} } @$sh3_got ], \@sh3_expected, 
    q[query with task => ...]);
cmp_deeply([ map { $_->{task_objects} } @$sh3_got ],
    array_each(
	array_each(isa("VCS::CMSynergy::Object") & methods(cvtype => "task")) & 
	superbagof($task6)),
    q[check task_objects keyword]);

my $rel_got = $task6->is_associated_cv_of;
cmp_deeply($rel_got, array_each(isa("VCS::CMSynergy::Object")), 
    q[is_associated_cv_of() returns array of V::C::O]);
cmp_vcos($rel_got, \@sh3_expected,
    q[VCO::is_associated_cv_of]);

my $fd_got = $task6->has_task_in_folder;
cmp_deeply($fd_got, 
    array_each(isa("VCS::CMSynergy::Object") & methods(cvtype => "folder")),
    q[VCO::has_task_in_folder contains only folders]);


my $complex_expected = 
[
  [ 'task-1:cvtype:base',	'base/cvtype/task/1' ],
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

# NOTE: Exclude tasks as they are unpredictable (automatic tasks in
# general, also other test may create tasks that tyhe can't clean up).
# Also exlude baselines, releasedefs, and recon_temps (CM Synergy >= 6.3 only).
my $complex_got = $ccm->query_arrayref(
    "name match 't*' and not ( cvtype = 'task' or cvtype = 'baseline' or cvtype = 'recon_temp' or cvtype = 'releasedef' )", 
    qw(objectname displayname));
verbose('complex_got', $complex_got);
cmp_bag($complex_got, $complex_expected,
    q[query "name match 't*' ..."]);
cmp_deeply($complex_got, array_each(code(
    sub 
    {
	my $got = shift;
	my ($objectname, $displayname) = @$got;
	my $prop = $ccm->property(displayname => $objectname);
	return 1 if $prop eq $displayname;
	return (0, "property vs keyword: $prop != $displayname");
    }
    )), q[property "displayname" == query keyword "displayname"]);

# test query with attribute caching
my @bc_attrs = qw(create_time owner status_log super_type);
my $bc_got = $ccm->query_object_with_attributes($b_query, @bc_attrs);
verbose('bc_got', $bc_got);
cmp_vcos($bc_got, \@b_expected, qq[query_object_with_attributes()]);
# check for actually cached attributes (type specific)
cmp_deeply($bc_got, array_each(code(
    sub
    {
	my $got = shift;
	my $acache = $got->_private->{acache};
	my $list = $got->list_attributes;
	foreach my $attr (@bc_attrs)
	{
	    return (0, "$got: attribute $attr not cached")
		if defined($list->{$attr}) && !defined($acache->{$attr});
	}
	return 1;
    }
    )), qq[attributes are actually cached]);

$ccm = undef;	# workaround for "seek() on closed filehandle $fh" in IPC::Run3

exit 0;

