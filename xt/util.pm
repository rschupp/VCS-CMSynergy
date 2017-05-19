# Copyright (c) 2001-2015 argumentum GmbH
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

use strict;
use warnings;

use lib '.';
use t::util;

use Test::Deep 0.093;

### Test::Deep stuff

sub vco		
{ 
    (my $objectname = shift) =~ s/-/:/;         # normalize delim to ":"
    return isa("VCS::CMSynergy::Object") & methods(objectname => $objectname);
}

# compare sets of objects (with useful diagnostics)
sub cmp_vcos	{ return cmp_deeply($_[0], vcoset($_[1]), $_[2]); }
sub vcoset	{ return Test::Deep::SetOfVCO->new(@_); }

package Test::Deep::SetOfVCO;  			# cf Test::Deep::Set

use Test::Deep::Cmp;

sub init
{
    my $self = shift;
    my $exp = shift;

    # $exp must be either a reference to an array of strings (objectnames)
    # or a reference to a hash with objectnames as keys and a hash
    # of attributes (name => value) as values (or undef)
    if (ref $exp eq "HASH")
    {
	while (my ($k, $v) = each %$exp)
	{
	    next unless defined $v;
	    die "No hashref for attributes of `$k' supplied, got `$v' instead" 
		unless ref $v eq "HASH";
	}
	$self->{val} = $exp;
    }
    elsif (ref $exp eq "ARRAY")
    {
	my %exp;
	foreach (@$exp)
	{
	    die "Not a string (objectname) supplied, got `$_' instead"
		if ref $_;
	    $exp{$_} = undef;
	}
	$self->{val} = \%exp;
    }
    else
    {
	die "No hashref or arrayref supplied, got `$exp' instead";
    }
}


sub descend
{
    my $self = shift;
    my $d1 = shift;
    my $exp = $self->{val};

    my $diag = "";
    if (ref $d1 ne "ARRAY")
    {
	my $val = Test::Deep::render_val($d1);
	$diag = <<EOM;
got    : $val
expect : an array of VCS::CMSynergy::Objects
EOM
    }

    my %got;
    if (not $diag)
    {
	my $bad = "";
	for (my $i = 0; $i < @$d1; $i++)
	{
	    my $obj = $d1->[$i];
	    unless (UNIVERSAL::isa($obj, "VCS::CMSynergy::Object"))
	    {
		my $val = Test::Deep::render_val($obj);
		$bad .= " [$i]=$val";
		next;
	    }
	    $got{$obj->objectname} = $obj;
	}
	$diag .= <<EOM if $bad;
got    : bad elements$bad
expect : only VCS::CMSynergy::Objects
EOM
    }

    if (not $diag)
    {
	my @missing = sort grep { !exists $got{$_} } keys %$exp;
	$diag .= "Missing objects: @missing\n" if @missing;

	my @extra = sort grep { !exists $exp->{$_} } keys %got;
	$diag .= "Extra objects: @extra\n" if @extra;

	if (VCS::CMSynergy::use_cached_attributes())
	{
	    foreach (sort grep { exists $exp->{$_} } keys %got)
	    {
		my $attrs = $exp->{$_} or next;
		my $obj = $got{$_};

		local $Test::Deep::Stack = Test::Deep::Stack->new;
		local $Test::Deep::CompareCache = Test::Deep::Cache->new;

		unless (Test::Deep::superhashof($attrs)->descend(
		    $obj->_private->{acache}))
		{
		    my $message = Test::Deep::deep_diag($Test::Deep::Stack);
		    $diag .= <<EOM;
Comparing cached attributes for object $_
$message
EOM
		}
	    }
	}
    }

    if ($diag)
    {
	    $self->data->{diag} = $diag;
	    return 0;
    }
    else
    {
	    return 1;
    }
}


sub diagnostics
{
    my $self = shift;
    my ($where, $last) = @_;

    my $diag = <<EOM;
Comparing $where as a set of VCS::CMSynergy::Objects
$last->{diag}
EOM

    return $diag;
}

# what's the purpose of this (for Test::Deep)?
sub compare
{
    die "compare called";
}

1;
