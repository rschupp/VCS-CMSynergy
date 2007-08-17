#!/usr/bin/perl

use Test::More tests => 4;
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

exit 0;
