#!/usr/bin/perl

use Test::More tests => 50;
use t::util;
use strict;

BEGIN 
{ 
    my @use = (':tied_objects');
    push @use, ':cached_attributes' if $ENV{CCM_USE_CACHED_ATTRIBUTES};
    use_ok('VCS::CMSynergy', @use); 
    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless $ENV{CCM_USE_CACHED_ATTRIBUTES};
	ok(VCS::CMSynergy::use_cached_attributes(), q[using :cached_attributes]);
    }
}

my @cleanup;			# cleanup actions
END { &{ pop @cleanup } while @cleanup; }


my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
diag("using cached_attributes") if VCS::CMSynergy::use_cached_attributes();

# we need a modifiable object...
my ($rc, $out, $err) = $ccm->folder(qw/-create -name/, "the great quux");
ok($rc == 0, q[create folder]);
my ($fno) = $out =~ /Created folder (\d+)\./;
ok(defined $fno, q[expected ouput]);
my $folder = $fno . $ccm->delimiter . "1:folder:probtrac";
push @cleanup, sub { $ccm->folder(qw/-delete -quiet -y/, $folder) };

my @values=
(
    gmtime()." the quick brown fox jumps over the lazy dog", # simple string
    gmtime().join("-" x 10, 1..100),		# long string
    join("\n", gmtime(), 1..10),		# string with newlines
    join(" ", map { qq["$_"] } gmtime(), 1..10), # string with embedded quotes
);

my $fobj = $ccm->object($folder);
isa_ok($fobj, "VCS::CMSynergy::Object");
is($fobj->objectname, $folder, q[check objectname()]);

# test with V::C::O methods
ok($ccm->create_attribute("blurfl", text => "initial value", $folder), q[create attribute]);
ok(defined $ccm->get_attribute(blurfl => $folder), q[attribute was created]);
ok(exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really created]);

foreach my $value (@values)
{
    ok($fobj->set_attribute(blurfl => $value),
	q[set_attribute (V::C::O)]);
    is($fobj->get_attribute('blurfl'), $value,
	q[re-get_attribute (V::C::O) and compare]);
    is($ccm->get_attribute(blurfl => $folder), $value,
	q[re-get_attribute (V::C) and compare]);
    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless VCS::CMSynergy::use_cached_attributes();
	is($fobj->_private->{acache}->{blurfl}, $value,
	    q[check cached attribute value]);
    }
}
is($fobj->property("displayname"), $fno,
    q[check for expected displayname with V::C::O::property()]);
is($fobj->displayname, $fno,
    q[check for expected displayname with V::C::O::displayname()]);

ok($fobj->delete_attribute("blurfl"), q[delete attribute]);
ok(!exists $fobj->list_attributes->{blurfl}, q[attribute was deleted]);
ok(!exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really deleted]);
SKIP:
{
    skip "not using :cached_attributes", 1 
	unless VCS::CMSynergy::use_cached_attributes();
    ok(!defined $fobj->_private->{acache}->{blurfl}, q[attribute no longer cached]);
}

# retest with tied object methods
ok($fobj->create_attribute("frobozz", text => "initial value"), q[create attribute]);
ok(defined $fobj->{frobozz}, q[attribute was created]);
ok(exists $fobj->list_attributes->{frobozz}, q[attribute was really created]);

foreach my $value (@values)
{
    ok($fobj->{frobozz} = $value,
	q[set_attribute (tied hash)]);
    is($fobj->{frobozz}, $value,
	q[re-get_attribute (tied hash) and compare]);
    is($ccm->get_attribute(frobozz => $folder), $value,
	q[re-get_attribute (V::C) and compare]);
}

is($fobj->property("displayname"), $fno,
    q[check for expected displayname with tied hash property()]);
is($fobj->displayname, $fno,
    q[check for expected displayname with tied hash displayname()]);

my @attrs1 = keys %{ $ccm->list_attributes($folder) };
my @attrs2 = keys %$fobj;
ok(eq_array(\@attrs1, \@attrs2), q[V::C vs tied hash list_attributes()]);

exit 0;
