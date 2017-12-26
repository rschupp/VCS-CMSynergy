#!/usr/bin/perl -w

use Test::More tests => 64;
use End;
use lib '.';
use xt::util;
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

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
diag("using :cached_attributes") if VCS::CMSynergy::use_cached_attributes();

sub _acache { shift->[VCS::CMSynergy::Object::ACACHE()] }

# Note: Even though the Synergy CLI does report a non-existent attribute
# as an error, it is caught by get_attribute() and undef is returned.
# This makes retrieving an attribute's value explicitly via 
# "ccm attribute -show"  and implicitly via a %keyword in a format
# (when using :cached_attributes) return the same result even for
# a non-existent attribute.
my $frobozz = eval { $ccm->get_attribute(frobozz => $ccm->base_model); };
ok(!$@,               "get_attribute() of non-existent attribute doesn't throw exception");
ok(!defined $frobozz, "get_attribute() of non-existent attribute returns undef");


my %props_exp =
(
    "calculator:1.0:project:1" =>
    {
        "status" => "released",
        "status_log" => "Wed Aug 13 15:26:09 1997: Status set to 'working' by ccm_root in role ccm_admin\nWed Aug 13 16:29:00 1997: Status set to 'released' by ccm_root in role ccm_admin"
    },
    "calculator:int:project:1" =>
    {
        "status" => "prep",
        "status_log" => "Wed Aug 13 16:33:19 1997: Status set to 'working' by ccm_root in role ccm_admin\nWed Aug 13 16:33:31 1997: Status set to 'prep' by ccm_root in role ccm_admin"
    },
    "calculator:darcy:project:1" =>
    {
        "status" => "working",
        "status_log" => "Wed Aug 13 16:43:24 1997: Status set to 'working' by darcy in role developer"
    },
    "calculator:int_20021125:project:1" =>
    {
        "status" => "integrate",
        "status_log" => "Mon Nov 25 18:36:31 2002: Status set to 'working' by ccm_root in role build_mgr\nMon Nov 25 18:36:32 2002: Status set to 'prep' by ccm_root in role build_mgr\nMon Nov 25 18:36:39 2002: Status set to 'integrate' by ccm_root in role build_mgr"
    },
);

# check that get_attribute() and query_*()/property()
# will return the same value wrt trailing newline
my $ppl_a = $ccm->base_model->get_attribute("project_purpose_list");
my $ppl_p = $ccm->base_model->property("project_purpose_list");
my ($ppl_q) = map { $_->{objectname} eq $ccm->base_model->objectname ? 
                      $_->{project_purpose_list} : () }
		@{ $ccm->query_hashref(
		    [ name => "base" ], qw/objectname project_purpose_list/) };
is($ppl_a, $ppl_p, "attribute value with trailing newline via attribute/property");
is($ppl_a, $ppl_q, "attribute value with trailing newline via attribute/query");

{
    # test set_attribute with different values
    # we need a modifiable object...
    my ($rc, $out, $err) = $ccm->folder(qw/-create -name/, gmtime()." the great quux");
    ok($rc == 0, q[create folder]);
    my $rx_created = qr/Created folder (.*?)\./;
    like($out, $rx_created, "Created folder ...");
    my ($fno) = $out =~ $rx_created;
    my $folder = $ccm->folder_object($fno);
    my $cleanup = end { $ccm->folder(qw/-delete -quiet -y/, $folder) };

    my @values=
    (
	[ "simple string" =>	gmtime()." the quick brown fox jumps over the lazy dog" ],
	[ "long string" =>		gmtime().join("-" x 10, 1..100) ],
	[ "multi-line string" =>	join("\n", scalar gmtime(), 1..10) ],
	[ "embedded quotes" =>	join(" ", map { qq["$_"] } scalar gmtime(), 1..10) ],
	[ "empty string"		=> "" ],
    );

    # test with V::C methods
    ok($ccm->create_attribute("blurfl", text => "initial value", $folder),
	q[create attribute blurfl]);
    ok(exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really created]);

    # NOTE: 2 tests per $value 
    foreach (@values)
    {
	SKIP:
	{
	    my ($desc, $value) = @$_;
	    skip "setting an attribute to an empty value doesn't work on Windows", 2
		    if VCS::CMSynergy::Client::is_win32 && $value eq "";
	    is($ccm->set_attribute(blurfl => $folder, $value), $value,
		qq[set_attribute (V::C): $desc]);
	    is($ccm->get_attribute(blurfl => $folder), $value,
		qq[re-get_attribute (V::C) and compare: $desc]);
	}
    }
    is($ccm->property(displayname => $folder), $fno,
	q[check for expected displayname with V::C::property()]);

    ok($ccm->delete_attribute("blurfl", $folder), q[delete attribute]);
    ok(!exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really deleted]);

    # retest with V::C::O methods
    ok($folder->create_attribute("blurfl", "text", "initial value"),
	q[create attribute]);
    ok(exists $folder->list_attributes->{blurfl}, q[attribute was created]);
    ok(exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really created]);
    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless VCS::CMSynergy::use_cached_attributes();
	ok(exists _acache($folder)->{blurfl}, q[attribute was cached]);
    }

    # NOTE: 4 tests per $value 
    foreach (@values)
    {
	SKIP:
	{
	    my ($desc, $value) = @$_;
	    skip "setting an attribute to an empty value doesn't work on Windows", 4
		    if VCS::CMSynergy::Client::is_win32 && $value eq "";
	    is($folder->set_attribute(blurfl => $value), $value,
		qq[set_attribute (V::C::O): $desc]);
	    is($folder->get_attribute('blurfl'), $value,
		qq[re-get_attribute (V::C::O) and compare: $desc]);
	    is($ccm->get_attribute(blurfl => $folder), $value,
		qq[re-get_attribute (V::C) and compare: $desc]);
	    SKIP:
	    {
		skip "not using :cached_attributes", 1 
		    unless VCS::CMSynergy::use_cached_attributes();
		is(_acache($folder)->{blurfl}, $value,
		    qq[check cached attribute value: $desc]);
	    }
	}
    }

    ok($folder->delete_attribute("blurfl"), q[delete attribute]);
    ok(!exists $folder->list_attributes->{blurfl}, q[attribute was deleted]);
    ok(!exists $ccm->list_attributes($folder)->{blurfl}, q[attribute was really deleted]);
    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless VCS::CMSynergy::use_cached_attributes();
	ok(!defined _acache($folder)->{blurfl}, q[attribute no longer cached]);
    }

    # check that $obj->set_attribute implicitly does "ccm attr -create -force"
    # for an inherited attribute
    my $inherited = "type_description";
    is($ccm->attribute_origin($folder, $inherited), "inherited", 
	qq[attribute $inherited is inherited]);
    my $inherited_value = $folder->get_attribute($inherited);
    my $local_value = "forced to local";
    ok(defined $folder->set_attribute($inherited => $local_value),
	q[V::C::O::set_attribute() works for inherited attribute]);
    is($ccm->attribute_origin($folder, $inherited), "local", 
	qq[attribute $inherited is now local]);
    is($folder->get_attribute($inherited), $local_value,
	q[check local attribute value]);
    ok($folder->delete_attribute($inherited),
	q[delete local attribute]);
    is($ccm->attribute_origin($folder, $inherited), "inherited", 
	qq[attribute $inherited reverted to inherited]);
    is($folder->get_attribute($inherited), $inherited_value,
	q[check inherited attribute value]);


    is($folder->property("displayname"), $fno,
	q[check for expected displayname with V::C::O::property()]);
    is($folder->displayname, $fno,
	q[check for expected displayname with V::C::O::displayname()]);
}


{
    my @keywords = qw/ status status_log /;
    my $props_got = $ccm->properties_hashref([ keys %props_exp ], @keywords);
    cmp_deeply($props_got, [ values %props_exp ], "properties_hashref");


    my @objs = map { $ccm->object($_) } keys %props_exp;
    $props_got = $ccm->properties_object([ keys %props_exp ], @keywords);
    cmp_deeply($props_got, [ map { vco($_) } keys %props_exp ], "properties_object: objects");

    SKIP:
    {
	skip "not using :cached_attributes", 1 
	    unless $ENV{CCM_USE_CACHED_ATTRIBUTES};
        $_->_forget_acache foreach @objs;
        cmp_deeply([ map { hash_slice(_acache($_), @keywords) } @$props_got ],
                   [ values %props_exp ], "properties_object: cached attributes");
    }
}


exit 0;

sub hash_slice
{
    my $hash = shift;
    my %slice;
    @slice{@_} = @$hash{@_};
    return \%slice;
}

package VCS::CMSynergy;

sub attribute_origin
{
    my ($self, $file_spec, $attr_name) = @_;

    my ($rc, $out, $err) = $self->_ccm(qw/attribute -la/, $file_spec);
    return undef unless $rc == 0;

    # NOTE: regex works for both classic mode and web mode
    my %attrs = $out =~ /^(\S+?) [\s(]+ .*? [\s()]+ (.*?) [\s)]/gmx; 
    return $attrs{$attr_name};
}

