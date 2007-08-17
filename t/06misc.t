#!/usr/bin/perl

use Test::More tests => 9;
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# test types()
my @types_expected;
for (scalar $ccm->version)
{
    /^4\.5|^5\./ && do { 
	@types_expected = qw(
	    ascii binary c++ csrc dir executable incl library lsrc
	    makefile project relocatable_obj shared_library shsrc ysrc);
	push @types_expected, "symlink" unless $VCS::CMSynergy::Is_MSWin32;
	last;
    };

    /^6\./ && do { 
	@types_expected = qw(
	    ascii binary bitmap c++ class csrc css dir dtd
	    executable gif html incl jar java jpeg library lsrc
	    makefile perl project relocatable_obj shared_library
	    shsrc xml ysrc);
	push @types_expected, "symlink" unless $VCS::CMSynergy::Is_MSWin32;
	last;
    };

    die "don't know anything about CM Synergy version $_";
}
my @types_got = $ccm->types;
verbose('types_got', \@types_got);
ok(eq_set(\@types_expected, \@types_got), q[$ccm->types]);

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

my @trav1_expected = qw(
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
my @trav1_got;
$ccm->traverse_project(
  {
    wanted => sub {
      push @trav1_got, 
        join("/", map { $_->name } @VCS::CMSynergy::Traversal::dirs, $_)
	  unless $_->cvtype eq 'project';
      },
    subprojects	=> 1,
    preprocess	=> sub { sort { $a->name cmp $b->name } @_; },
  },
  'toolkit-1.0:project:1');
ok(eq_array(\@trav1_got, \@trav1_expected),
  q[traverse_project with sub projects, entryies sorted by name]);

my @trav2_expected = 
(
  'misc-1:dir:1',
  'toolkit.ini-1:ascii:1',
  'readme-1:ascii:1',
);
my @trav2_got;
$ccm->traverse_project(
  sub { push @trav2_got, $_; },
  'toolkit-1.0:project:1', $ccm->object('misc-1:dir:1'));
all_ok { UNIVERSAL::isa($_, 'VCS::CMSynergy::Object'); } \@trav2_got,
  q[traverse_project with start directory];
ok(eq_set(strobjs(\@trav2_got), \@trav2_expected),
  q[traverse_project with start directory]);

my @trav3_expected = 
(
  'toolkit-1.0:project:1',
  'editor-1.0:project:1',
  'guilib-1.0:project:1',
  'calculator-1.0:project:1',
);
my @trav3_got;
$ccm->traverse_project(
  {
    wanted => sub { push @trav3_got, $_ if $_->cvtype eq 'project'; },
    subprojects	=> 1,
  },
  'toolkit-1.0:project:1');
all_ok { $_->cvtype eq 'project' } \@trav3_got,
  q[traverse_project for all sub projects];
ok(eq_set(strobjs(\@trav3_got), \@trav3_expected),
  q[traverse_project for all sub projects]);

exit 0;
