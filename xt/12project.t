#!/usr/bin/perl

use Test::More tests => 32;
use xt::util;
use strict;

BEGIN 
{ 
    use_ok('VCS::CMSynergy', ':cached_attributes'); 
    ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
}

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $top_project = $ccm->object('toolkit-1.0:project:1');
isa_ok($top_project, "VCS::CMSynergy::Project");

my %project_tree_expected = (
  tree_0_0 =>
  {
    subprojects => 0,
    mark_projects => 0,
    expected =>
    {
      'toolkit' => vco('toolkit-1:dir:1'),
      'toolkit/makefile' => vco('makefile-1:makefile:1'),
      'toolkit/makefile.pc' => vco('makefile.pc-1:makefile:1'),
      'toolkit/misc' => vco('misc-1:dir:1'),
      'toolkit/misc/readme' => vco('readme-1:ascii:1'),
      'toolkit/misc/toolkit.ini' => vco('toolkit.ini-1:ascii:1'),
    }
  },
  tree_1_0 =>
  {
    subprojects => 1,
    mark_projects => 0,
    expected =>
    {
      'toolkit' => vco('toolkit-1:dir:1'),
      'toolkit/calculator' => vco('calculator-1:dir:1'),
      'toolkit/calculator/calculator' => vco('calculator-1:executable:1'),
      'toolkit/calculator/calculator.exe' => vco('calculator.exe-1:executable:1'),
      'toolkit/calculator/includes' => vco('includes-1:dir:4'),
      'toolkit/calculator/includes/clear.h' => vco('clear.h-1:incl:1'),
      'toolkit/calculator/includes/math.h' => vco('math.h-1:incl:1'),
      'toolkit/calculator/makefile' => vco('makefile-1:makefile:4'),
      'toolkit/calculator/makefile.pc' => vco('makefile.pc-1:makefile:4'),
      'toolkit/calculator/sources' => vco('sources-1:dir:3'),
      'toolkit/calculator/sources/clear.c' => vco('clear.c-1:csrc:2'),
      'toolkit/calculator/sources/main.c' => vco('main.c-1:csrc:4'),
      'toolkit/calculator/sources/math.c' => vco('math.c-1:csrc:1'),
      'toolkit/editor' => vco('editor-1:dir:1'),
      'toolkit/editor/editor' => vco('editor-1:executable:1'),
      'toolkit/editor/editor.exe' => vco('editor.exe-1:executable:1'),
      'toolkit/editor/includes' => vco('includes-1:dir:2'),
      'toolkit/editor/includes/delete.h' => vco('delete.h-1:incl:1'),
      'toolkit/editor/includes/save.h' => vco('save.h-1:incl:1'),
      'toolkit/editor/makefile' => vco('makefile-1:makefile:2'),
      'toolkit/editor/makefile.pc' => vco('makefile.pc-1:makefile:2'),
      'toolkit/editor/sources' => vco('sources-1:dir:1'),
      'toolkit/editor/sources/delete.c' => vco('delete.c-1:csrc:1'),
      'toolkit/editor/sources/main.c' => vco('main.c-1:csrc:2'),
      'toolkit/editor/sources/save.c' => vco('save.c-1:csrc:1'),
      'toolkit/guilib' => vco('guilib-1:dir:1'),
      'toolkit/guilib/guilib.a' => vco('guilib.a-1:library:1'),
      'toolkit/guilib/guilib.lib' => vco('guilib.lib-1:library:1'),
      'toolkit/guilib/includes' => vco('includes-1:dir:3'),
      'toolkit/guilib/includes/controls.h' => vco('controls.h-1:incl:1'),
      'toolkit/guilib/includes/guilib.h' => vco('guilib.h-1:incl:1'),
      'toolkit/guilib/makefile' => vco('makefile-1:makefile:3'),
      'toolkit/guilib/makefile.pc' => vco('makefile.pc-1:makefile:3'),
      'toolkit/guilib/sources' => vco('sources-1:dir:2'),
      'toolkit/guilib/sources/controls.c' => vco('controls.c-1:csrc:1'),
      'toolkit/guilib/sources/main.c' => vco('main.c-1:csrc:3'),
      'toolkit/makefile' => vco('makefile-1:makefile:1'),
      'toolkit/makefile.pc' => vco('makefile.pc-1:makefile:1'),
      'toolkit/misc' => vco('misc-1:dir:1'),
      'toolkit/misc/readme' => vco('readme-1:ascii:1'),
      'toolkit/misc/toolkit.ini' => vco('toolkit.ini-1:ascii:1'),
    }
  },
  tree_0_1 =>
  {
    subprojects => 0,
    mark_projects => 1,
    expected =>
    {
      'toolkit' => vco('toolkit-1.0:project:1'),
      'toolkit/calculator' => vco('calculator-1.0:project:1'),
      'toolkit/editor' => vco('editor-1.0:project:1'),
      'toolkit/guilib' => vco('guilib-1.0:project:1'),
      'toolkit/makefile' => vco('makefile-1:makefile:1'),
      'toolkit/makefile.pc' => vco('makefile.pc-1:makefile:1'),
      'toolkit/misc' => vco('misc-1:dir:1'),
      'toolkit/misc/readme' => vco('readme-1:ascii:1'),
      'toolkit/misc/toolkit.ini' => vco('toolkit.ini-1:ascii:1'),
    }
  },
  tree_1_1 =>
  {
    subprojects => 1,
    mark_projects => 1,
    expected =>
    {
      'toolkit' => vco('toolkit-1.0:project:1'),
      'toolkit/calculator' => vco('calculator-1.0:project:1'),
      'toolkit/calculator/calculator' => vco('calculator-1:executable:1'),
      'toolkit/calculator/calculator.exe' => vco('calculator.exe-1:executable:1'),
      'toolkit/calculator/includes' => vco('includes-1:dir:4'),
      'toolkit/calculator/includes/clear.h' => vco('clear.h-1:incl:1'),
      'toolkit/calculator/includes/math.h' => vco('math.h-1:incl:1'),
      'toolkit/calculator/makefile' => vco('makefile-1:makefile:4'),
      'toolkit/calculator/makefile.pc' => vco('makefile.pc-1:makefile:4'),
      'toolkit/calculator/sources' => vco('sources-1:dir:3'),
      'toolkit/calculator/sources/clear.c' => vco('clear.c-1:csrc:2'),
      'toolkit/calculator/sources/main.c' => vco('main.c-1:csrc:4'),
      'toolkit/calculator/sources/math.c' => vco('math.c-1:csrc:1'),
      'toolkit/editor' => vco('editor-1.0:project:1'),
      'toolkit/editor/editor' => vco('editor-1:executable:1'),
      'toolkit/editor/editor.exe' => vco('editor.exe-1:executable:1'),
      'toolkit/editor/includes' => vco('includes-1:dir:2'),
      'toolkit/editor/includes/delete.h' => vco('delete.h-1:incl:1'),
      'toolkit/editor/includes/save.h' => vco('save.h-1:incl:1'),
      'toolkit/editor/makefile' => vco('makefile-1:makefile:2'),
      'toolkit/editor/makefile.pc' => vco('makefile.pc-1:makefile:2'),
      'toolkit/editor/sources' => vco('sources-1:dir:1'),
      'toolkit/editor/sources/delete.c' => vco('delete.c-1:csrc:1'),
      'toolkit/editor/sources/main.c' => vco('main.c-1:csrc:2'),
      'toolkit/editor/sources/save.c' => vco('save.c-1:csrc:1'),
      'toolkit/guilib' => vco('guilib-1.0:project:1'),
      'toolkit/guilib/guilib.a' => vco('guilib.a-1:library:1'),
      'toolkit/guilib/guilib.lib' => vco('guilib.lib-1:library:1'),
      'toolkit/guilib/includes' => vco('includes-1:dir:3'),
      'toolkit/guilib/includes/controls.h' => vco('controls.h-1:incl:1'),
      'toolkit/guilib/includes/guilib.h' => vco('guilib.h-1:incl:1'),
      'toolkit/guilib/makefile' => vco('makefile-1:makefile:3'),
      'toolkit/guilib/makefile.pc' => vco('makefile.pc-1:makefile:3'),
      'toolkit/guilib/sources' => vco('sources-1:dir:2'),
      'toolkit/guilib/sources/controls.c' => vco('controls.c-1:csrc:1'),
      'toolkit/guilib/sources/main.c' => vco('main.c-1:csrc:3'),
      'toolkit/makefile' => vco('makefile-1:makefile:1'),
      'toolkit/makefile.pc' => vco('makefile.pc-1:makefile:1'),
      'toolkit/misc' => vco('misc-1:dir:1'),
      'toolkit/misc/readme' => vco('readme-1:ascii:1'),
      'toolkit/misc/toolkit.ini' => vco('toolkit.ini-1:ascii:1'),
    }
  },
);

while (my ($what, $t) = each %project_tree_expected)
{
  my $trav_tree_got = $ccm->project_tree(
    { 
      subprojects	=> $t->{subprojects}, 
      mark_projects	=> $t->{mark_projects}, 
      pathsep		=> "/" ,
    }, 
    $top_project);
  cmp_deeply($trav_tree_got, $t->{expected},
    qq[project_tree: compare results for $what]);
}


my @trav_paths_expected = sort keys %{ $project_tree_expected{tree_1_0}->{expected} };
my @trav_depths_expected = map { tr:/:/: } @trav_paths_expected;
my @trav_objects_expected = (
    @{ $top_project->recursive_is_member_of },
    $top_project);		# because recursive_is_member_of() does NOT include the project itself

# dir and project stack expected for 'save.c-1:csrc:1' 
# (toolkit/editor/sources/save.c) in $top_project 
my @dirs_expected = (
  vco('toolkit-1:dir:1'),		# toolkit
  vco('editor-1:dir:1'),		# toolkit/editor
  vco('sources-1:dir:1'),		# toolkit/editor/sources
);
my @projects_expected = (
  vco('toolkit-1.0:project:1'),		# toolkit
  vco('editor-1.0:project:1'),		# toolkit/editor
);
my (@dirs_got, @projects_got);

my (@trav_path_got, @trav_depth_got, @trav_object_got, $trav_tree_expected);
my ($npre, $npost);
ok($top_project->traverse(
  {
    wanted => sub {
      push @trav_object_got, $_;
      return if $_->is_project;

      my $path = VCS::CMSynergy::Traversal::path();
      push @trav_path_got, $path;
      $trav_tree_expected->{$path} = $_;
      push @trav_depth_got, VCS::CMSynergy::Traversal::depth();

      if ($_->objectname eq 'save.c:1:csrc:1')
      {
	  @dirs_got = @VCS::CMSynergy::Traversal::dirs;
	  @projects_got = @VCS::CMSynergy::Traversal::projects;
      }
    },
    subprojects	=> 1,
    pathsep 	=> "/",
    preprocess	=> sub { $npre++; return sort { $a->name cmp $b->name } @_; },
    postprocess => sub { $npost++; }
  }),
  qq[traverse $top_project]);

cmp_vcos(\@trav_object_got, [ map { $_->objectname } @trav_objects_expected ],
  q[traverse: check objects]);
cmp_deeply(\@trav_path_got, \@trav_paths_expected,
  q[traverse: check pathnames]);
cmp_deeply(\@trav_depth_got, \@trav_depths_expected,
  q[traverse: check depth]);
ok($npre == $npost, 
  q[traverse: compare number of preprocess and postprocess calls]);
ok($npre == (grep { $_->cvtype =~ /^(dir|project)$/ } @trav_objects_expected),
  q[traverse: compare number of preprocess calls to projects/dirs traversed]);
cmp_deeply(\@dirs_got, \@dirs_expected, 
  q[traverse: check @VCS::CMSynergy::Traversal::dirs]);
cmp_deeply(\@projects_got, \@projects_expected, 
  q[traverse: check @VCS::CMSynergy::Traversal::projects]);
is(scalar @VCS::CMSynergy::Traversal::dirs, 0,
  q[@VCS::CMSynergy::Traversal::dirs is empty outside of traverse]);
is(scalar @VCS::CMSynergy::Traversal::projects, 0,
  q[@VCS::CMSynergy::Traversal::projects is empty outside of traverse]);

my $dir2 = $ccm->object('misc-1:dir:1');
my @trav2_expected = (
    $dir2->objectname, 
    qw(toolkit.ini:1:ascii:1 readme:1:ascii:1)
);
my @trav2_got;
$top_project->traverse(
  sub { push @trav2_got, $_; },
  $dir2);
cmp_vcos(\@trav2_got, \@trav2_expected, 
    qq[traverse($top_project) starting from $dir2]);

my @trav3_expected = grep { $_->is_project } @trav_objects_expected;
my @trav3_got;
$top_project->traverse(
  {
    wanted => sub { push @trav3_got, $_ if $_->is_project; },
    subprojects	=> 1,
  });
cmp_bag(objectnames(\@trav3_got), objectnames(\@trav3_expected),
  qq[traverse($top_project) with subprojects]);


my $children_got = $top_project->is_child_of($dir2);
cmp_vcos($children_got, [ grep { !/:dir:/ } @trav2_expected ],
    qq[is_child_of($dir2)]);

my $obj_from_path = $top_project->object_from_path([ qw/toolkit misc readme/ ]);
cmp_vcos([ $obj_from_path ], [ 'readme:1:ascii:1' ],
    qq[object_from_path()]);


## get_member_info
my $have_get_member_info = eval
{
    my ($rc, $out, $err);

    # $ ccm define
    # define -> corecmds:cmd_define
    # alias -> corecmds:cmd_alias
    # unalias -> corecmds:cmd_unalias
    # ...
    ($rc, $out, $err) = $ccm->_ccm(qw/define/);
    return 0 unless $rc == 0;

    unless ($out =~ m{^get_member_info\s}m)
    {
	# get_member_info not yet defined, check whether intlib.a is loaded

	# $ ccm load	
	# Interface libraries:
	# 	/opt/Telelogic/ccm64/lib/guilib.a
	# 	...
	# 	/var/lib/telelogic/ccm64/cs_test/db/../lib/apilib.a
	# 	...
	($rc, $out, $err) = $ccm->_ccm(qw/load/);
	return 0 unless $rc == 0;

	unless ($out =~ m{[/\\]intlib\.a$}m)
	{
	    # intlib.a not loaded, try to load it from $CCM_HOME/lib
	    ($rc, $out, $err) = $ccm->_ccm(load => File::Spec->catfile($ccm->ccm_home, qw/lib intlib.a/));
	    return 0 unless $rc == 0;
	}

	($rc, $out, $err) = $ccm->_ccm(qw/define get_member_info intcmds get_member_info_cmd/);
	return 0 unless $rc == 0;
    }
    return 1;
};

SKIP: 
{
    skip "optional get_member_info command not present (see README.get_member_info)", 2
	unless $have_get_member_info;

    my $mi_expected =
    {
      'toolkit/misc/readme' => 
      {
	objectname => 'readme-1:ascii:1',
	status => 'released',
	status_log => 'Wed Aug 13 15:13:57 1997: Status set to \'working\' by ccm_root in role ccm_admin
Wed Aug 13 16:27:36 1997: Status set to \'released\' by ccm_root in role ccm_admin'
      },
      'toolkit/makefile' => 
      {
	objectname => 'makefile-1:makefile:1',
	status => 'released',
	status_log => 'Wed Aug 13 15:13:42 1997: Status set to \'working\' by ccm_root in role ccm_admin
Wed Aug 13 16:27:26 1997: Status set to \'released\' by ccm_root in role ccm_admin'
      },
      'toolkit/misc/toolkit.ini' => 
      {
	objectname => 'toolkit.ini-1:ascii:1',
	status => 'released',
	status_log => 'Wed Aug 13 15:13:54 1997: Status set to \'working\' by ccm_root in role ccm_admin
Wed Aug 13 16:27:33 1997: Status set to \'released\' by ccm_root in role ccm_admin'
      },
      'toolkit/makefile.pc' => 
      {
	objectname => 'makefile.pc-1:makefile:1',
	status => 'released',
	status_log => 'Wed Aug 13 15:13:47 1997: Status set to \'working\' by ccm_root in role ccm_admin
Wed Aug 13 16:27:29 1997: Status set to \'released\' by ccm_root in role ccm_admin'
      }
    };
    my $mi_got = $top_project->get_member_info_hashref(qw/objectname status status_log/, { subprojects => 0, pathsep => "/" });
    cmp_deeply($mi_got, $mi_expected, 
	qq[get_member_info_hashref($top_project)]);

    my $mi_subprojects_expected =
    {
      'calculator/calculator' => vco('calculator-1:executable:1'),
      'calculator/calculator.exe' => vco('calculator.exe-1:executable:1'),
      'calculator/includes/clear.h' => vco('clear.h-1:incl:1'),
      'calculator/includes/math.h' => vco('math.h-1:incl:1'),
      'calculator/makefile' => vco('makefile-1:makefile:4'),
      'calculator/makefile.pc' => vco('makefile.pc-1:makefile:4'),
      'calculator/sources/clear.c' => vco('clear.c-1:csrc:2'),
      'calculator/sources/main.c' => vco('main.c-1:csrc:4'),
      'calculator/sources/math.c' => vco('math.c-1:csrc:1'),
      'editor/editor' => vco('editor-1:executable:1'),
      'editor/editor.exe' => vco('editor.exe-1:executable:1'),
      'editor/includes/delete.h' => vco('delete.h-1:incl:1'),
      'editor/includes/save.h' => vco('save.h-1:incl:1'),
      'editor/makefile' => vco('makefile-1:makefile:2'),
      'editor/makefile.pc' => vco('makefile.pc-1:makefile:2'),
      'editor/sources/delete.c' => vco('delete.c-1:csrc:1'),
      'editor/sources/main.c' => vco('main.c-1:csrc:2'),
      'editor/sources/save.c' => vco('save.c-1:csrc:1'),
      'guilib/guilib.a' => vco('guilib.a-1:library:1'),
      'guilib/guilib.lib' => vco('guilib.lib-1:library:1'),
      'guilib/includes/controls.h' => vco('controls.h-1:incl:1'),
      'guilib/includes/guilib.h' => vco('guilib.h-1:incl:1'),
      'guilib/makefile' => vco('makefile-1:makefile:3'),
      'guilib/makefile.pc' => vco('makefile.pc-1:makefile:3'),
      'guilib/sources/controls.c' => vco('controls.c-1:csrc:1'),
      'guilib/sources/main.c' => vco('main.c-1:csrc:3'),
      'toolkit/makefile' => vco('makefile-1:makefile:1'),
      'toolkit/makefile.pc' => vco('makefile.pc-1:makefile:1'),
      'toolkit/misc/readme' => vco('readme-1:ascii:1'),
      'toolkit/misc/toolkit.ini' => vco('toolkit.ini-1:ascii:1'),
    };
    my $mi_subprojects_got = $top_project->get_member_info_object({ subprojects => 1, pathsep => "/" });
    cmp_deeply($mi_subprojects_got, $mi_subprojects_expected, 
	qq[get_member_info_object($top_project) with subprojects]);
}

my $prep_project = $ccm->object('toolkit-int:project:1');

my $pg = $prep_project->project_grouping;
isa_ok($pg, 'VCS::CMSynergy::ProjectGrouping');
cmp_deeply($pg, vco('Toolkit%002f2.0%003aintegrate-1:project_grouping:1'), "project_grouping");

my @pg_projects_exp = qw(
  calculator:int:project:1
  editor:int:project:1
  guilib:int:project:1
  toolkit:int:project:1
);
my $pg_projects_got = $pg->show_object('projects');
cmp_vcos($pg_projects_got, \@pg_projects_exp, "project_grouping: show projects");

my $pg_baseline_got = $pg->show_object('baseline');
cmp_deeply($pg_baseline_got, [vco('toolkit_2.0_INT_1-1:baseline:1')], "project_grouping: show baseline");

my @pg_folders_exp = qw(
  4:1:folder:probtrac
);
my $pg_folders_got = $pg->show_object('folders');
cmp_vcos($pg_folders_got, \@pg_folders_exp, "project_grouping: show baseline");

my $pr = $prep_project->process_rule;
isa_ok($pr, 'VCS::CMSynergy::ProcessRule');
cmp_deeply($pr, vco('Toolkit%003aIntegration Testing:2.0:process_rule:1'), "process_rules");

my @pr_folder_templates_exp = (
  'all completed tasks for release %release:1:folder_temp:1',
);
my $pr_folder_templates_got = $pr->show_object('folder_templates');
cmp_vcos($pr_folder_templates_got, \@pr_folder_templates_exp, "process_rule: show folder_templates");

exit(0);
