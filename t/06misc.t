#!/usr/bin/perl

use Test::More tests => 29;
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
ok(!$ccm->object('frobozz.c-1:csrc:1')->exists, q[object doesn't exist]);

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

my $project = 'toolkit-1.0:project:1';
my @trav_path_expected = qw(
  toolkit
  toolkit/calculator
  toolkit/calculator/calculator
  toolkit/calculator/calculator.exe
  toolkit/calculator/includes
  toolkit/calculator/includes/clear.h
  toolkit/calculator/includes/math.h
  toolkit/calculator/makefile
  toolkit/calculator/makefile.pc
  toolkit/calculator/sources
  toolkit/calculator/sources/clear.c
  toolkit/calculator/sources/main.c
  toolkit/calculator/sources/math.c
  toolkit/editor
  toolkit/editor/editor
  toolkit/editor/editor.exe
  toolkit/editor/includes
  toolkit/editor/includes/delete.h
  toolkit/editor/includes/save.h
  toolkit/editor/makefile
  toolkit/editor/makefile.pc
  toolkit/editor/sources
  toolkit/editor/sources/delete.c
  toolkit/editor/sources/main.c
  toolkit/editor/sources/save.c
  toolkit/guilib
  toolkit/guilib/guilib.a
  toolkit/guilib/guilib.lib
  toolkit/guilib/includes
  toolkit/guilib/includes/controls.h
  toolkit/guilib/includes/guilib.h
  toolkit/guilib/makefile
  toolkit/guilib/makefile.pc
  toolkit/guilib/sources
  toolkit/guilib/sources/controls.c
  toolkit/guilib/sources/main.c
  toolkit/makefile
  toolkit/makefile.pc
  toolkit/misc
  toolkit/misc/readme
  toolkit/misc/toolkit.ini
);
my @trav_object_expected = @{ $ccm->query_object(
    { recursive_is_member_of => [ $project, 'none' ] }) };
push @trav_object_expected, $ccm->object($project);	
# because recursive_is_member_of() does NOT include the project itself

my (@trav_path_got, @trav_object_got, $trav_tree_expected, $preprocess, $postprocess);
ok($ccm->traverse_project(
  {
    wanted => sub {
      push @trav_object_got, $_;
      return if $_->is_project;

      my $path = join("/", map { $_->name } @VCS::CMSynergy::Traversal::dirs, $_);
      push @trav_path_got, $path;
      $trav_tree_expected->{$path} = $_;
    },
    subprojects	=> 1,
    preprocess	=> sub { $preprocess++; return sort { $a->name cmp $b->name } @_; },
    postprocess => sub { $postprocess++; }
  },
  $project), qq[traverse_project($project)]);

ok(eq_set(objectnames(\@trav_object_got), objectnames(\@trav_object_expected)),
  q[traverse_project: check objects]);
ok(eq_array(\@trav_path_got, \@trav_path_expected),
  q[traverse_project: check pathnames]);
ok($preprocess == $postprocess, 
  q[traverse_project: compare number of preprocess and postprocess calls]);
ok($preprocess == (grep { $_->cvtype =~ /^(dir|project)$/ } @trav_object_expected),
  q[traverse_project: compare number of preprocess calls to projects/dirs traversed]);

my $trav_tree_got = $ccm->project_tree(
  { subprojects => 1, pathsep => "/" }, $project);
isa_ok($trav_tree_got, "HASH", qq[project_tree($project)]);
ok(eq_hash($trav_tree_expected, $trav_tree_got),
  q[project_tree: compare results]);

my @trav2_expected = 
(
  'misc-1:dir:1',
  'toolkit.ini-1:ascii:1',
  'readme-1:ascii:1',
);
my @trav2_got;
$ccm->traverse_project(
  sub { push @trav2_got, $_; },
  $project, $ccm->object('misc-1:dir:1'));
all_ok { UNIVERSAL::isa($_, 'VCS::CMSynergy::Object'); } \@trav2_got,
  q[traverse_project with start directory];
ok(eq_set(objectnames(\@trav2_got), \@trav2_expected),
  q[traverse_project with start directory]);

my @trav3_expected = grep { $_->is_project } @trav_object_expected;
my @trav3_got;
$ccm->traverse_project(
  {
    wanted => sub { push @trav3_got, $_ if $_->is_project; },
    subprojects	=> 1,
  },
  $project);
all_ok { $_->is_project } \@trav3_got,
  q[traverse_project for all sub projects];
ok(eq_set(objectnames(\@trav3_got), objectnames(\@trav3_expected)),
  q[traverse_project for all sub projects]);


BEGIN { use_ok('VCS::CMSynergy::Users'); }

my $users = $ccm->users;
isa_ok($users, 'HASH', q[return value of users()]);
all_ok { UNIVERSAL::isa($_, 'ARRAY') } [ values %$users ],
  q[users() returns HASH of ARRAY refs];
ok(exists $users->{ccm_root}, q[ccm_root is in users]);

exit 0;
