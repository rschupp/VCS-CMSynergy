#!/usr/bin/perl

use Test::More tests => 2;
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using Expect") if defined $ccm->{exp};

my @types_expected;
for (scalar $ccm->version)
{
    /^4\.5|^5\./ && do { @types_expected = qw(
	    ascii binary c++ csrc dir executable incl library lsrc
	    makefile project relocatable_obj shared_library
	    shsrc symlink ysrc
    ), last };

    /^6\./ && do { @types_expected = qw(
	    ascii binary bitmap c++ class csrc css dir dtd
	    executable gif html incl jar java jpeg library lsrc
	    makefile perl project relocatable_obj shared_library
	    shsrc xml ysrc
    ), last };

    die "don't know anything about CM Synergy version $_";
}
my @types_got = $ccm->types;
verbose('types_got', \@types_got);
ok(eq_set(\@types_expected, \@types_got), q[$ccm->types]);

exit 0;
