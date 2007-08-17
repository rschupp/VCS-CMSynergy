#!/usr/bin/perl

use Test::More tests => 2;
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using Expect") if defined $ccm->{exp};

my $ccm_version = $ccm->version;
SKIP: {
    my %types_exp = (
	"5.1" => [ qw(
	    ascii binary c++ csrc dir executable incl library lsrc
	    makefile project relocatable_obj shared_library
	    shsrc symlink ysrc
	)],
	"6.2" => [ qw(
	    ascii binary bitmap c++ class csrc css dir dtd
	    executable gif html incl jar java jpeg library lsrc
	    makefile perl project relocatable_obj shared_library
	    shsrc xml ysrc
	)],
    );

    skip "dunno anything about CM Synergy version $ccm_version", 1 
        unless exists $types_exp{$ccm_version};

    my @types_result = $ccm->types;
    verbose('types_result', \@types_result);
    ok(eq_set($types_exp{$ccm_version}, \@types_result),
       q[$ccm->types]);
}

exit 0;
