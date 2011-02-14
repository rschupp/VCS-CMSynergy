#!/usr/bin/perl

use Test::More tests => 6;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

# convert project reference from Unix pathnames to native pathnames
# NOTE: We can't use File::Spec here since Cygwin uses slash as the path
# delimiter, but CM Synergy on Windows returns backslashes 
# in project references.
sub native_path
{
    local $_ = shift;
    s{/}{\\}g if VCS::CMSynergy::Client::is_win32;
    return $_;
}
	
my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $q_expected63 = 
[
  {
    objectname => 'main.c-1:csrc:1',
    finduse => {}
  },
  {
    objectname => 'main.c-2:csrc:1',
    finduse => {}
  },
  {
    objectname => 'main.c-1:csrc:2',
    finduse => 
    {
       'editor-1.0:project:1' => native_path('editor/sources/main.c')
    }
  },
  {
    objectname => 'main.c-1:csrc:3',
    finduse => 
    {
       'guilib-int_20021125:project:1' => native_path('guilib/sources/main.c'),
       'guilib-1.0:project:1' => native_path('guilib/sources/main.c'),
       'guilib-int:project:1' => native_path('guilib/sources/main.c'),
       'guilib-darcy:project:1' => native_path('guilib/sources/main.c')
    }
  },
  {
    objectname => 'main.c-1:csrc:4',
    finduse => 
    {
       'calculator-darcy:project:1' => native_path('calculator/sources/main.c'),
       'calculator-1.0:project:1' => native_path('calculator/sources/main.c'),
       'calculator-int:project:1' => native_path('calculator/sources/main.c'),
       'calculator-int_20021125:project:1' => native_path('calculator/sources/main.c')
    }
  },
  {
    objectname => 'main.c-2:csrc:2',
    finduse => 
    {
       'editor-int_20021125:project:1' => native_path('editor/sources/main.c'),
       'editor-int:project:1' => native_path('editor/sources/main.c'),
       'editor-darcy:project:1' => native_path('editor/sources/main.c')
    }
  }
];
my $q_expected51 = 
[
  {
    objectname => 'main.c-1:csrc:1',
    finduse => {}
  },
  {
    objectname => 'main.c-2:csrc:1',
    finduse => {}
  },
  {
    objectname => 'main.c-1:csrc:2',
    finduse => 
    {
       'editor-1.0:project:1' => native_path('editor/sources/main.c'),
       'editor-int:project:1' => native_path('editor/sources/main.c')
    }
  },
  {
    objectname => 'main.c-2:csrc:2',
    finduse => 
    {
       'editor-darcy:project:1' => native_path('editor/sources/main.c')
    }
  },
  {
    objectname => 'main.c-1:csrc:3',
    finduse => 
    {
       'guilib-1.0:project:1' => native_path('guilib/sources/main.c'),
       'guilib-int:project:1' => native_path('guilib/sources/main.c'),
       'guilib-darcy:project:1' => native_path('guilib/sources/main.c')
    }
  },
  {
    objectname => 'main.c-1:csrc:4',
    finduse => 
    {
       'calculator-1.0:project:1' => native_path('calculator/sources/main.c'),
       'calculator-int:project:1' => native_path('calculator/sources/main.c'),
       'calculator-darcy:project:1' => native_path('calculator/sources/main.c')
    }
  }
];
my $q_expected = $ccm->version >= 6.3 ? $q_expected63 : $q_expected51;

my $q_got = $ccm->query_hashref("name = 'main.c'", qw(objectname finduse));
verbose('q_got', $q_got);
cmp_bag($q_got, $q_expected, 
   q[$ccm->query_hashref("name = 'main.c'", qw(objectname finduse))]);
   
my $fu_expected = [ map { [ $_->{objectname}, $_->{finduse} ] } @$q_expected ];
my $old_format = $ccm->set(Object_format => "%objectname");
my $fu_got = $ccm->finduse(map { $_->{objectname} } @$q_expected);
verbose('fu_got', $fu_got);
$ccm->set(Object_format => $old_format);
cmp_bag($fu_got, $fu_expected, q[$ccm->finduse(...)]);

is($ccm->findpath("main.c-1:csrc:3", "guilib-darcy:project:1"),
   native_path("guilib/sources/main.c"),
   q[$ccm->findpath("main.c-1:csrc:3", "guilib-darcy")]);
ok(!defined $ccm->findpath("main.c-1:csrc:3", "blurfl"),
   q[$ccm->findpath("main.c-1:csrc:3", "blurfl") -- no match]);

exit 0;
