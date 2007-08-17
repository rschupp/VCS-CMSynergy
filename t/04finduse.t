#!/usr/bin/perl

use Test;
BEGIN { plan tests => 9 }
use t::util;

# convert project reference from Unix pathnames to native pathnames
# NOTE: We can't use File::Spec here since Cygwin uses "/", but
# CM Synergy on NT returns "\" in project references.
sub projref2native
{
    local $_ = shift;
    s{/}{\\}g if $^O eq 'MSWin32' || $^O eq 'cygwin';
    return $_;
}
	
my $ccm = VCS::CMSynergy->new(%test_session);
ok(ref $ccm, 'VCS::CMSynergy');
print "# using Expect\n" if $ccm->{exp};

my $fu_exp = [
   [
     'main.c-2:csrc:2',
     'editor/sources/main.c-2@editor-darcy'
   ],
   [
     'main.c-1:csrc:2',
     'editor/sources/main.c-1@editor-1.0',
     'editor/sources/main.c-1@editor-int'
   ],
   [
     'main.c-1:csrc:3',
     'guilib/sources/main.c-1@guilib-1.0',
     'guilib/sources/main.c-1@guilib-int',
     'guilib/sources/main.c-1@guilib-darcy'
   ],
   [
     'main.c-1:csrc:4',
     'calculator/sources/main.c-1@calculator-1.0',
     'calculator/sources/main.c-1@calculator-int',
     'calculator/sources/main.c-1@calculator-darcy'
   ],
   [
     'main.c-2:csrc:1'
   ],
   [
     'main.c-1:csrc:1'
   ]
];
$_ = [ shift @$_, map { projref2native($_) } @$_ ] foreach (@$fu_exp); 

my $old_format = $ccm->set("Object_format", "%objectname");
my $fu_result = $ccm->finduse(map { $_->[0] } @$fu_exp);
verbose('fu_result', $fu_result);
$ccm->set("Object_format", $old_format);
ok(ref $fu_result, 'ARRAY');
ok(all { ref $_ eq 'ARRAY' } @$fu_result);
ok(eqarray(
    $fu_exp, $fu_result, 
    sub { $a->[0] cmp $b->[0] },		# sort on first array element
    sub {
	my @a = @{ $_[0] };
	my @b = @{ $_[1] };
	return 0 unless shift @a eq shift @b;	# objectname
	return eqarray(\@a, \@b);		# list of proj_refs
    }));

my $q_exp = [
  {
    'objectname' => 'main.c-1:csrc:1',
    'finduse' => []
  },
  {
    'objectname' => 'main.c-2:csrc:1',
    'finduse' => []
  },
  {
    'objectname' => 'main.c-1:csrc:2',
    'finduse' => [
		   'editor/sources/main.c-1@editor-1.0',
		   'editor/sources/main.c-1@editor-int'
		 ]
  },
  {
    'objectname' => 'main.c-2:csrc:2',
    'finduse' => [
		   'editor/sources/main.c-2@editor-darcy'
		 ]
  },
  {
    'objectname' => 'main.c-1:csrc:3',
    'finduse' => [
		   'guilib/sources/main.c-1@guilib-1.0',
		   'guilib/sources/main.c-1@guilib-int',
		   'guilib/sources/main.c-1@guilib-darcy'
		 ]
  },
  {
    'objectname' => 'main.c-1:csrc:4',
    'finduse' => [
		   'calculator/sources/main.c-1@calculator-1.0',
		   'calculator/sources/main.c-1@calculator-int',
		   'calculator/sources/main.c-1@calculator-darcy'
		 ]
  }
];
$_->{finduse} = [ map { projref2native($_) } @{ $_->{finduse} } ] foreach (@$q_exp); 

my $q_result = $ccm->query_hashref("name = 'main.c'", qw(objectname finduse));
verbose('q_result', $q_result);
ok(ref $q_result, 'ARRAY');
ok(all { ref $_ eq 'HASH' } @$q_result);
ok(eqarray(
    $q_exp, $q_result,
    sub { $a->{objectname} cmp $b->{objectname} }, # sort on key objectname
    sub { eqhash($_[0], $_[1], finduse => \&eqarray) }
    ));
   
ok($ccm->findpath("main.c-1:csrc:3", "guilib-darcy"), 
   projref2native("guilib/sources/main.c"));
ok(!defined $ccm->findpath("main.c-1:csrc:3", "blurfl"));

exit 0;
