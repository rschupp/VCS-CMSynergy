#!/usr/bin/perl

use Test;
BEGIN { plan tests => 7 }
use t::util;

my ($ccm_addr, $ccm_test_db);
{
    # create a new CM Synergy session
    my $ccm = VCS::CMSynergy->new(%test_session);
    ok(ref $ccm, 'VCS::CMSynergy');
    print "# using Expect\n" if defined $ccm->{exp};
    $ccm_addr = $ccm->{CCM_ADDR};

    # new session should show up in `ccm status'
    my $status = VCS::CMSynergy->status;
    ok(ref $status, 'ARRAY');
    my ($entry) = grep { $_->{rfc_address} eq $ccm_addr } @$status;
    ok($entry->{database} eq $ccm->{database});

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(
	    CCM_ADDR	=> $ccm_addr,
	    PrintError	=> 0,
	    RaiseError	=> 1,
    );
    ok(ref $ccm2, 'VCS::CMSynergy');
    ok($ccm2->{CCM_ADDR} eq $ccm_addr);

    # destroy it and check that the CM Synergy session is still there
    $ccm2->DESTROY;
    ok(grep { $_->{rfc_address} eq $ccm_addr } @$status);

    # $ccm goes out of scope and session should be stopped
}

# session should now longer be listed in `ccm status'
ok(all { $_->{rfc_address} ne $ccm_addr } @{ VCS::CMSynergy->status });

exit 0;
