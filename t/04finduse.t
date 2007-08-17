#!/usr/bin/perl

use Test::More tests => 10;
use t::util;

# convert project reference from Unix pathnames to native pathnames
# NOTE: We can't use File::Spec here since Cygwin uses "/" as the path
# delimiter, but CM Synergy on NT returns "\" in project references.
sub unix2native
{
    local $_ = shift;
    s{/}{\\}g if $^O eq 'MSWin32' || $^O eq 'cygwin';
    return $_;
}
	
my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using Expect") if defined $ccm->{exp};

my $q_expected = [
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
		   'editor-1.0'		=> 'editor/sources/main.c',
		   'editor-int'		=> 'editor/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-2:csrc:2',
    'finduse' => {
		   'editor-darcy'	=> 'editor/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:3',
    'finduse' => {
		   'guilib-1.0'		=> 'guilib/sources/main.c',
		   'guilib-int'		=> 'guilib/sources/main.c',
		   'guilib-darcy'	=> 'guilib/sources/main.c'
		 }
  },
  {
    'objectname' => 'main.c-1:csrc:4',
    'finduse' => {
		   'calculator-1.0'	=> 'calculator/sources/main.c',
		   'calculator-int'	=> 'calculator/sources/main.c',
		   'calculator-darcy'	=> 'calculator/sources/main.c'
		 }
  }
];
foreach my $exp (@$q_expected)
{
    $_ = unix2native($_) foreach (values %{ $exp->{finduse} });
}

my $q_got = $ccm->query_hashref("name = 'main.c'", qw(objectname finduse));
verbose('q_got', $q_got);
isa_ok($q_got, "ARRAY", "query_hashref() returns array ref");
ok(all(sub { UNIVERSAL::isa($_, "HASH") }, @$q_got),
   q[query_hashref() returns array ref of hash refs]);
ok(all(sub { UNIVERSAL::isa($_->{finduse}, "HASH") }, @$q_got),
   q[query_hashref(): keyword 'finduse' returns hash ref]);
ok(eq_array(		# sort on key objectname
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_expected ],
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_got ]),
   q[$ccm->query_hashref("name = 'main.c'", qw(objectname finduse))]);
   
my $old_format = $ccm->set("Object_format", "%objectname");
my $fu_got = $ccm->finduse(map { $_->{objectname} } @$q_expected);
verbose('fu_got', $fu_got);
$ccm->set("Object_format", $old_format);
isa_ok($fu_got, "ARRAY", "finduse() returns array ref");
ok(all(sub { 
     UNIVERSAL::isa($_, "ARRAY") && @$_ == 2 && UNIVERSAL::isa($_->[1], "HASH") }, 
   @$fu_got),
   q[finduse() returns array ref of [$description, %uses]]);
ok(eq_array(		# sort on first array element (objectname)
   [ sort { $a->[0] cmp $b->[0] } map { [ $_->{objectname}, $_->{finduse} ] } @$q_expected ],
   [ sort { $a->[0] cmp $b->[0] } @$fu_got ]),
   q[$ccm->finduse(...)]);

is($ccm->findpath("main.c-1:csrc:3", "guilib-darcy"),
   unix2native("guilib/sources/main.c"),
   q[$ccm->findpath("main.c-1:csrc:3", "guilib-darcy")]);
ok(!defined $ccm->findpath("main.c-1:csrc:3", "blurfl"),
   q[$ccm->findpath("main.c-1:csrc:3", "blurfl") -- no match]);

exit 0;
