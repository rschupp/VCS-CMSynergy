#!/usr/bin/perl

use Test::More tests => 17;
use t::util;

my ($ccm_addr, $ps);

{
    # create a new CM Synergy session
    my $ccm = VCS::CMSynergy->new(%test_session);
    isa_ok($ccm, "VCS::CMSynergy");
    diag("using coprocess") if defined $ccm->{coprocess};
    $ccm_addr = $ccm->ccm_addr;

    # new session should show up in `ccm ps'
    $ps = VCS::CMSynergy->ps(rfc_address => $ccm_addr);
    isa_ok($ps, "ARRAY", q[ps()]);
    is(@$ps, 1, 
       qq[VCS::CMSynergy->ps(rfc_address => $ccm_addr) is array of length 1]);
    is($ps->[0]->{database}, $ccm->database,
       q[Database gleaned from ps should match that from session]);

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    isa_ok($ccm2, "VCS::CMSynergy");
    is($ccm2->ccm_addr, $ccm_addr, 
       qq[Attached session has same CCM_ADDR as original session $ccm_addr]);

    # destroy session object 
    $ccm2 = undef;

    # check that the CM Synergy session is still there
    ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
       qq[Original session $ccm_addr is still registered]);

    # $ccm goes out of scope and session should be stopped
}
# session should no longer show up in `ccm ps'
ok(!@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
   qq[Original session $ccm_addr is not registered any more]);

{
    # create a new CM Synergy session with KeepSession on
    my $ccm = VCS::CMSynergy->new(%test_session, KeepSession => 1);
    isa_ok($ccm, "VCS::CMSynergy");
    $ccm_addr = $ccm->ccm_addr;
    ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
       qq[New session $ccm_addr with KeepSession "on" is registered]);

    # destroy session object
    $ccm = undef;

    # check that the CM Synergy session is still there
    ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
       qq[Session $ccm_addr is still registered]);

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    isa_ok($ccm2, "VCS::CMSynergy");
    is($ccm2->ccm_addr, $ccm_addr, 
       qq[Attached session has same CCM_ADDR as original session $ccm_addr]);

    # destroy session object 
    $ccm2 = undef;

    # check that the CM Synergy session is still there
    ok(@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
       qq[Original session $ccm_addr is still registered]);
    # destroy it and check that the CM Synergy session is still there

    # create another session object reusing the CM Synergy session,
    # but with KeepSession off
    my $ccm3 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    KeepSession	=> 0,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    isa_ok($ccm3, "VCS::CMSynergy");
    is($ccm3->ccm_addr, $ccm_addr, 
       qq[Attached session has same CCM_ADDR as original session $ccm_addr]);

    # $ccm3 goes out of scope and session should be stopped
}
# session should no longer show up in `ccm ps'
ok(!@{ VCS::CMSynergy->ps(rfc_address => $ccm_addr) },
   qq[Original session $ccm_addr is not registered any more]);

# FIXME test: simultaneous session using second user (Windows or ESD only)?

exit 0;
