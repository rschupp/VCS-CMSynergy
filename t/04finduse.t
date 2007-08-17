#!/usr/bin/perl

use Test::More tests => 11;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

# convert project reference from Unix pathnames to native pathnames
# NOTE: We can't use File::Spec here since Cygwin uses "/" as the path
# delimiter, but CM Synergy on NT returns "\" in project references.
sub unix2native
{
    local $_ = shift;
    s{/}{\\}g if VCS::CMSynergy::Client::is_win32;
    return $_;
}
	
my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

my $q_expected = $ccm->version >= 6.3 ?
[
  {
    'objectname' => 'main.c-1:csrc:1',
    'finduse' => {}
  },
  {
    'objectname' => 'main.c-2:csrc:1',
    'finduse' => {}
  },
  {
    'objectname' => 'main.c-1:csrc:2',
    'finduse' => {
		   'editor-1.0:project:1' => 'editor/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:3',
    'finduse' => {
		   'guilib-int_20021125:project:1' => 'guilib/sources/main.c',
		   'guilib-1.0:project:1' => 'guilib/sources/main.c',
		   'guilib-int:project:1' => 'guilib/sources/main.c',
		   'guilib-darcy:project:1' => 'guilib/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:4',
    'finduse' => {
		   'calculator-darcy:project:1' => 'calculator/sources/main.c',
		   'calculator-1.0:project:1' => 'calculator/sources/main.c',
		   'calculator-int:project:1' => 'calculator/sources/main.c',
		   'calculator-int_20021125:project:1' => 'calculator/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-2:csrc:2',
    'finduse' => {
		   'editor-int_20021125:project:1' => 'editor/sources/main.c',
		   'editor-int:project:1' => 'editor/sources/main.c',
		   'editor-darcy:project:1' => 'editor/sources/main.c'
		 }
  }
] : [
  {
    'objectname' => 'main.c-1:csrc:1',
    'finduse' => {}
  },
  {
    'objectname' => 'main.c-2:csrc:1',
    'finduse' => {}
  },
  {
    'objectname' => 'main.c-1:csrc:2',
    'finduse' => {
		   'editor-1.0:project:1' => 'editor/sources/main.c',
		   'editor-int:project:1' => 'editor/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-2:csrc:2',
    'finduse' => {
		   'editor-darcy:project:1' => 'editor/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:3',
    'finduse' => {
		   'guilib-1.0:project:1' => 'guilib/sources/main.c',
		   'guilib-int:project:1' => 'guilib/sources/main.c',
		   'guilib-darcy:project:1' => 'guilib/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:4',
    'finduse' => {
		   'calculator-1.0:project:1' => 'calculator/sources/main.c',
		   'calculator-int:project:1' => 'calculator/sources/main.c',
		   'calculator-darcy:project:1' => 'calculator/sources/main.c'
		 }
  }
];
foreach my $exp (@$q_expected)
{
    $_ = unix2native($_) foreach values %{ $exp->{finduse} };
}

my $q_got = $ccm->query_hashref("name = 'main.c'", qw(objectname finduse));
verbose('q_got', $q_got);
isa_ok($q_got, "ARRAY", q[query_hashref()]);
all_ok { UNIVERSAL::isa($_, "HASH") } $q_got,
   q[query_hashref(): isa HASH];
all_ok { UNIVERSAL::isa($_->{finduse}, "HASH") } $q_got,
   q[query_hashref(): query keyword "finduse" isa HASH];
ok(eq_array(		# eq_set doesn't properly cope with refs
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_got ], 
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_expected ]), 
   q[$ccm->query_hashref("name = 'main.c'", qw(objectname finduse))]);
   
my $old_format = $ccm->set("Object_format", "%objectname");
my $fu_got = $ccm->finduse(map { $_->{objectname} } @$q_expected);
verbose('fu_got', $fu_got);
$ccm->set("Object_format", $old_format);
isa_ok($fu_got, "ARRAY", q[finduse()]);
all_ok 
    { 
	UNIVERSAL::isa($_, "ARRAY") 
        && @$_ == 2 
	&& UNIVERSAL::isa($_->[1], "HASH") 
    } $fu_got,
    q[finduse(): is [$description, \%uses]];
ok(eq_array(		# eq_set doesn't properly cope with refs
   [ sort { $a->[0] cmp $b->[0] } @$fu_got ], 
   [ sort { $a->[0] cmp $b->[0] } map { [ $_->{objectname}, $_->{finduse} ] } @$q_expected ]),
   q[finduse(): expected results]);

is($ccm->findpath("main.c-1:csrc:3", "guilib-darcy:project:1"),
   unix2native("guilib/sources/main.c"),
   q[$ccm->findpath("main.c-1:csrc:3", "guilib-darcy")]);
ok(!defined $ccm->findpath("main.c-1:csrc:3", "blurfl"),
   q[$ccm->findpath("main.c-1:csrc:3", "blurfl") -- no match]);

exit 0;
