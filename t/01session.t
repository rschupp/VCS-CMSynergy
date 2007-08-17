#!/usr/bin/perl

use Test::More tests => 8;
use t::util;

my ($ccm_addr, $ps);
{
    # create a new CM Synergy session
    my $ccm = VCS::CMSynergy->new(%test_session);
    isa_ok($ccm, "VCS::CMSynergy");
    diag("using Expect") if defined $ccm->{exp};
    $ccm_addr = $ccm->{CCM_ADDR};

    # new session should show up in `ccm ps'
    $ps = VCS::CMSynergy->ps(rfc_address => $ccm_addr);
    isa_ok($ps, "ARRAY");
    ok(@$ps == 1, 
       q[VCS::CMSynergy->ps(rfc_address => $ccm_addr is array of length 1]);
    ok($ps->[0]->{database} eq $ccm->{database},
       q[database from ps should match that from session]);

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    isa_ok($ccm2, "VCS::CMSynergy");
    is($ccm2->{CCM_ADDR}, $ccm_addr, 
       "reused session has same CCM_ADDR");

    # destroy it and check that the CM Synergy session is still there
    $ccm2->DESTROY;
    ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) } == 1,
       q[session still registered]);

    # $ccm goes out of scope and session should be stopped
}

# session should no longer show up in `ccm ps'
ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) } == 0,
   q[session not registered any more]);

# FIXME test: simultaneous session using second user (Windows or ESD only)?

exit 0;
