#!/usr/bin/perl

use Test::More tests => 10;
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


# test error handling
my ($after_foo, $handled);
$ccm->{PrintError} = 0;
$ccm->{RaiseError} = 1;

$ccm->{HandleError} = undef;
($after_foo, $handled) = (0, 0);
eval
{
    $ccm->foo('bar');
    $after_foo++;
};
ok($@, q[died because of RaiseError]);
ok(!$after_foo, q[$after_foo++ not reached]);

$ccm->{HandleError} = sub { $handled++; 0; };
($after_foo, $handled) = (0, 0);
eval
{
    $ccm->foo('bar');
    $after_foo++;
};
ok($@, q[died because of RaiseError]);
ok(!$after_foo, q[$after_foo++ not reached]);
ok($handled, q[HandleError called]);

$ccm->{HandleError} = sub { $handled++; 1; };
($after_foo, $handled) = (0, 0);
eval
{
    $ccm->foo('bar');
    $after_foo++;
};
ok(!$@, q[didn't die because of RaiseError]);
ok($after_foo, q[$after_foo++ reached]);
ok($handled, q[HandleError called]);

exit 0;
