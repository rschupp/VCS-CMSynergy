#!/usr/bin/perl

use Test;
BEGIN { plan tests => 7 }
use t::util;

my ($ccm_addr, $ps);
{
    # create a new CM Synergy session
    my $ccm = VCS::CMSynergy->new(%test_session);
    ok(ref $ccm, 'VCS::CMSynergy');
    print "# using Expect\n" if defined $ccm->{exp};
    $ccm_addr = $ccm->{CCM_ADDR};

    # new session should show up in `ccm ps'
    $ps = VCS::CMSynergy->ps(rfc_address => $ccm_addr);
    ok(ref $ps eq 'ARRAY' && @$ps == 1);
    ok($ps->[0]->{database} eq $ccm->{database});

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    ok(ref $ccm2, 'VCS::CMSynergy');
    ok($ccm2->{CCM_ADDR} eq $ccm_addr);

    # destroy it and check that the original CM Synergy session is still there
    $ccm2->DESTROY;
    $ps = VCS::CMSynergy->ps(rfc_address => $ccm_addr);
    ok(ref $ps eq 'ARRAY' && @$ps == 1);

    # $ccm goes out of scope and session should be stopped
}

# session should no longer show up in `ccm ps'
$ps = VCS::CMSynergy->ps(rfc_address => $ccm_addr);
ok(ref $ps eq 'ARRAY' && @$ps == 0);

# FIXME test: simultaneous session using second user (Windows or ESD only)?

exit 0;
