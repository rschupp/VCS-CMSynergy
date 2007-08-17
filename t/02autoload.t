#!/usr/bin/perl

use Test::More tests => 3;
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using Expect") if defined $ccm->{exp};

# test that autoloaded methods spring into existence
ok(!exists &VCS::CMSynergy::foo);
{
    local $ccm->{RaiseError} = 0;
    $ccm->foo(qw(bar quux));
}
ok(exists &VCS::CMSynergy::foo, 
   q[VCS::CMSynergy::foo got autoloaded]);

exit 0;
