#!/usr/bin/perl

use Test::More tests => 11;
use lib '.';
use xt::util;
use strict;

BEGIN { use_ok('VCS::CMSynergy'); }

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};

# test error handling
my ($reached, $handled, $answer);
$ccm->{PrintError} = 0;
$ccm->{RaiseError} = 1;
sub VCS::CMSynergy::foo { shift->set_error("the answer is", undef, 42); }

# test RaiseError w/o HandleError
$ccm->{HandleError} = undef;
($reached, $handled, $answer) = (0, 0, undef);
eval
{
    $answer = $ccm->foo('bar');
    $reached++;
};
like($@, qr/the answer is/, q[died because of RaiseError]);
ok(!$reached, q[$reached++ not reached]);

# test RaiseError/HandleError interaction
$ccm->{HandleError} = sub { $handled++; 0; };
($reached, $handled, $answer) = (0, 0, undef);
eval
{
    $answer = $ccm->foo('bar');
    $reached++;
};
like($@, qr/the answer is/, q[died because of RaiseError]);
ok(!$reached, q[$reached++ not reached]);
ok($handled, q[HandleError called]);

$ccm->{HandleError} = sub { $handled++; 1; };
($reached, $handled, $answer) = (0, 0, undef);
eval
{
    $answer = $ccm->foo('bar');
    $reached++;
};
is($@, "", q[didn't die because of RaiseError]);
ok($reached, q[$reached++ reached]);
ok($handled, q[HandleError called]);
is($answer, 42, q[return value propagated]);

exit 0;

