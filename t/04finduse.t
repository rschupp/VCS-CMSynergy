#!/usr/bin/perl

use Test::More tests => 10;
use t::util;
use UNIVERSAL qw(isa);

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

my $q_exp = [
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
foreach my $exp (@$q_exp)
{
    $_ = unix2native($_) foreach (values %{ $exp->{finduse} });
}

my $q_result = $ccm->query_hashref("name = 'main.c'", qw(objectname finduse));
verbose('q_result', $q_result);
isa_ok($q_result, "ARRAY", "query_hashref() returns array ref");
ok(all(sub { isa($_, "HASH") }, @$q_result),
   q[query_hashref() returns array ref of hash refs]);
ok(all(sub { isa($_->{finduse}, "HASH") }, @$q_result),
   q[query_hashref(): keyword 'finduse' returns hash ref]);
ok(eq_array(		# sort on key objectname
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_exp ],
   [ sort { $a->{objectname} cmp $b->{objectname} } @$q_result ]),
   q[$ccm->query_hashref("name = 'main.c'", qw(objectname finduse))]);
   
my $old_format = $ccm->set("Object_format", "%objectname");
my $fu_result = $ccm->finduse(map { $_->{objectname} } @$q_exp);
verbose('fu_result', $fu_result);
$ccm->set("Object_format", $old_format);
isa_ok($fu_result, "ARRAY", "finduse() returns array ref");
ok(all(sub { 
     isa($_, "ARRAY") && @$_ == 2 && isa($_->[1], "HASH") }, 
   @$fu_result),
   q[finduse() returns array ref of [$description, %uses]]);
ok(eq_array(		# sort on first array element (objectname)
   [ sort { $a->[0] cmp $b->[0] } map { [ $_->{objectname}, $_->{finduse} ] } @$q_exp ],
   [ sort { $a->[0] cmp $b->[0] } @$fu_result ]),
   q[$ccm->finduse(...)]);

is($ccm->findpath("main.c-1:csrc:3", "guilib-darcy"),
   unix2native("guilib/sources/main.c"),
   q[$ccm->findpath("main.c-1:csrc:3", "guilib-darcy")]);
ok(!defined $ccm->findpath("main.c-1:csrc:3", "blurfl"),
   q[$ccm->findpath("main.c-1:csrc:3", "blurfl") -- no match]);

exit 0;
