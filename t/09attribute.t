#!/usr/bin/perl -w

use Test::More tests => 52;
use t::util;
use strict;

BEGIN 
{ 
    my @use = ();
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
diag("using :cached_attributes") if VCS::CMSynergy::use_cached_attributes();

# test set_attribute with different values
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
    ""						# empty string
);

# test with V::C methods
ok($ccm->create_attribute("blurfl", "text", "initial value", $folder),
    q[create attribute]);
ok(exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really created]);

# NOTE: 2 tests per $value 
foreach my $value (@values)
{
    SKIP:
    {
	skip "setting an attribute to an empty value doesn't work on Windows", 2
		if VCS::CMSynergy::Client::is_win32 && $value eq "";
	is($ccm->set_attribute(blurfl => $folder, $value), $value,
	    q[set_attribute (V::C)]);
	is($ccm->get_attribute(blurfl => $folder), $value,
	    q[re-get_attribute (V::C) and compare]);
    }
}
is($ccm->property(displayname => $folder), $fno,
    q[check for expected displayname with V::C::property()]);

ok($ccm->delete_attribute("blurfl", $folder), q[delete attribute]);
ok(!exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really deleted]);

# retest with V::C::O methods
my $fobj = $ccm->object($folder);
isa_ok($fobj, "VCS::CMSynergy::Object");
is($fobj->objectname, $folder, q[check objectname()]);

ok($fobj->create_attribute("blurfl", "text", "initial value"),
    q[create attribute]);
ok(exists $fobj->list_attributes->{blurfl}, q[attribute was created]);
ok(exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really created]);
SKIP:
{
    skip "not using :cached_attributes", 1 
	unless VCS::CMSynergy::use_cached_attributes();
    ok(exists $fobj->_private->{acache}->{blurfl}, q[attribute was cached]);
}

# NOTE: 4 tests per $value 
foreach my $value (@values)
{
    SKIP:
    {
	skip "setting an attribute to an empty value doesn't work on Windows", 4
		if VCS::CMSynergy::Client::is_win32 && $value eq "";
	is($fobj->set_attribute(blurfl => $value), $value,
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
    ok(!exists $fobj->_private->{acache}->{blurfl}, q[attribute no longer cached]);
}

exit 0;
