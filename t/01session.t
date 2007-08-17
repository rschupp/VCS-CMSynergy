#!/usr/bin/perl

use Test::More tests => 23;
use t::util;

use VCS::CMSynergy::Client;
use Config;
use File::Spec;

# repeat sanity check from Makefile.PL
my $ccm_exe = File::Spec->catfile($ENV{CCM_HOME}, "bin", "ccm$Config{_exe}");
ok(-x $ccm_exe || ($^O eq 'cygwin' && -e $ccm_exe), q[sanity check (executable $CCM_HOME/bin/ccm)]);

# test VCS::CMSynergy::Client
my $client = VCS::CMSynergy::Client->new(
    CCM_HOME	=> $ENV{CCM_HOME},
    PrintError	=> $test_session{PrintError},
    RaiseError	=> $test_session{RaiseError},
);

isa_ok($client, "VCS::CMSynergy::Client");
is($client->ccm_home, $ENV{CCM_HOME}, q[CCM_HOMEs match]);

my $ps = $client->ps;
isa_ok($ps, "ARRAY", q[return value of ps()]);
ok((grep { $_->{process} eq "router" } @$ps) == 1, q[ps: found router]);
ok((grep { $_->{process} eq "objreg" } @$ps) > 0, q[ps: found object registrar(s)]);

my $ccm_addr;
{
    # create a new CM Synergy session
    my $ccm = VCS::CMSynergy->new(%test_session);
    isa_ok($ccm, "VCS::CMSynergy");
    diag("using coprocess") if defined $ccm->{coprocess};
    $ccm_addr = $ccm->ccm_addr;

    # new session should show up in `ccm ps'
    $ps = $client->ps(rfc_address => $ccm_addr);
    is(@$ps, 1, 
       qq[ps(rfc_address => $ccm_addr) is array of length 1]);
    is($ps->[0]->{database}, $ccm->database,
       q[database gleaned from ps should match that from session]);

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(CCM_ADDR => $ccm_addr);
    isa_ok($ccm2, "VCS::CMSynergy");
    is($ccm2->ccm_addr, $ccm_addr, 
       qq[attached session has same CCM_ADDR as original session $ccm_addr]);

    # destroy session object 
    $ccm2 = undef;

    # check that the CM Synergy session is still there
    ok(@{ $client->ps(rfc_address => $ccm_addr) },
       qq[original session $ccm_addr is still registered]);

    # $ccm goes out of scope and session should be stopped
}
# session should no longer show up in `ccm ps'
ok(!@{ $client->ps(rfc_address => $ccm_addr) },
   qq[original session $ccm_addr is not registered any more]);

{
    # create a new CM Synergy session with KeepSession on
    my $ccm = VCS::CMSynergy->new(%test_session, KeepSession => 1);
    isa_ok($ccm, "VCS::CMSynergy");
    $ccm_addr = $ccm->ccm_addr;
    ok(@{ $client->ps(rfc_address => $ccm_addr) },
       qq[new session $ccm_addr with KeepSession "on" is registered]);

    # destroy session object
    $ccm = undef;

    # check that the CM Synergy session is still there
    ok(@{ $client->ps(rfc_address => $ccm_addr) },
       qq[session $ccm_addr is still registered]);

    # create another session object reusing the CM Synergy session
    my $ccm2 = VCS::CMSynergy->new(CCM_ADDR => $ccm_addr);
    isa_ok($ccm2, "VCS::CMSynergy");
    is($ccm2->ccm_addr, $ccm_addr, 
       qq[attached session has same CCM_ADDR as original session $ccm_addr]);

    # destroy session object 
    $ccm2 = undef;

    # check that the CM Synergy session is still there
    ok(@{ $client->ps(rfc_address => $ccm_addr) },
       qq[original session $ccm_addr is still registered]);
    # destroy it and check that the CM Synergy session is still there

    # create another session object reusing the CM Synergy session,
    # but with KeepSession off
    my $ccm3 = VCS::CMSynergy->new(CCM_ADDR => $ccm_addr, KeepSession => 0);
    isa_ok($ccm3, "VCS::CMSynergy");
    is($ccm3->ccm_addr, $ccm_addr, 
       qq[attached session has same CCM_ADDR as original session $ccm_addr]);

    # $ccm3 goes out of scope and session should be stopped
}
# session should no longer show up in `ccm ps'
ok(!@{ $client->ps(rfc_address => $ccm_addr) },
   qq[original session $ccm_addr is not registered any more]);

# create session using start()
my %session = %test_session;
delete @session{qw(CCM_HOME PrintError RaiseError)};
my $ccm = $client->start(%session);
isa_ok($ccm, "VCS::CMSynergy");

# FIXME test: simultaneous session using a second user (Windows or ESD only)?

exit 0;
