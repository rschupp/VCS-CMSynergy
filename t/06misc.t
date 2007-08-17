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

my $project = $ccm->object('toolkit-1.0:project:1');
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
my @trav_depth_expected = map { tr:/:/: } @trav_path_expected;
my @trav_object_expected = (
    $project,		# because recursive_is_member_of() does NOT include the project itself
    @{ $project->recursive_is_member_of }
);

my (@trav_path_got, @trav_depth_got, @trav_object_got, $trav_tree_expected);
my ($npre, $npost);
ok($project->traverse(
  {
    wanted => sub {
      push @trav_object_got, $_;
      return if $_->is_project;

      my $path = VCS::CMSynergy::Traversal::path("/");
      push @trav_path_got, $path;
      $trav_tree_expected->{$path} = $_;
      push @trav_depth_got, VCS::CMSynergy::Traversal::depth();
    },
    subprojects	=> 1,
    preprocess	=> sub { $npre++; return sort { $a->name cmp $b->name } @_; },
    postprocess => sub { $npost++; }
  }),
  qq[traverse $project]);

cmp_bag(\@trav_object_got, [ map { vco($_->objectname) } @trav_object_expected ],
  q[traverse: check objects]);
cmp_deeply(\@trav_path_got, \@trav_path_expected,
  q[traverse: check pathnames]);
cmp_deeply(\@trav_depth_got, \@trav_depth_expected,
  q[traverse: check depth]);
ok($npre == $npost, 
  q[traverse: compare number of preprocess and postprocess calls]);
ok($npre == (grep { $_->cvtype =~ /^(dir|project)$/ } @trav_object_expected),
  q[traverse: compare number of preprocess calls to projects/dirs traversed]);

my $trav_tree_got = $ccm->project_tree(
  { subprojects => 1, pathsep => "/" }, $project);
cmp_deeply($trav_tree_got, $trav_tree_expected,
  q[project_tree: compare results]);

my @trav2_expected = 
(
  'misc-1:dir:1',
  'toolkit.ini-1:ascii:1',
  'readme-1:ascii:1',
);
my @trav2_got;
$project->traverse(
  sub { push @trav2_got, $_; },
  $ccm->object('misc-1:dir:1'));
ok(are_vcos(\@trav2_got),
  q[visited by traverse: isa V::C::O]);
ok(eq_set(objectnames(\@trav2_got), \@trav2_expected),
  q[visited by traverse: check objects]);

my @trav3_expected = grep { $_->is_project } @trav_object_expected;
my @trav3_got;
$project->traverse(
  {
    wanted => sub { push @trav3_got, $_ if $_->is_project; },
    subprojects	=> 1,
  });
all_ok { $_->is_project } \@trav3_got,
  q[traverse(subprojects => 1): is_project];
ok(eq_set(objectnames(\@trav3_got), objectnames(\@trav3_expected)),
  q[traverse(subprojects => 1)]);


BEGIN { use_ok('VCS::CMSynergy::Users'); }

my $users = $ccm->users;
isa_ok($users, 'HASH', q[users()]);
cmp_deeply([ values %$users ], array_each(isa('ARRAY')), 
  q[users(): isa HASH of ARRAY]);
ok(exists $users->{ccm_root}, q[ccm_root is in users]);

exit 0;
