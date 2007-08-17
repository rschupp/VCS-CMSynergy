#!/usr/bin/perl

use Test::More tests => 4;
use t::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# test that autoloaded methods spring into existence
ok(!$ccm->can("foo"),	q[method foo not yet autoloaded]);
{
    local $ccm->{RaiseError} = 0;	# because there is no "ccm foo ..."
    $ccm->foo(qw(bar quux));
}
ok($ccm->can("foo"),	q[method foo has been autoloaded]);

exit 0;
