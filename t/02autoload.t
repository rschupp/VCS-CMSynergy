#!/usr/bin/perl

use Test;
BEGIN { plan tests => 3 }
use t::util;

my $ccm = VCS::CMSynergy->new(%test_session, RaiseError => 0);
ok(ref $ccm, 'VCS::CMSynergy');
print "# using Expect\n" if $ccm->{exp};

# test that autoloaded methods spring into existence
ok(!exists &VCS::CMSynergy::foo);
$ccm->foo(qw(bar quux));
ok(exists &VCS::CMSynergy::foo);

exit 0;
