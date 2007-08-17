use Data::Dumper;
use Test::Deep 0.093;

our %test_session;

BEGIN
{
    die "CCM_HOME not set in environment" unless $ENV{CCM_HOME};
    die "CCM_TEST_DB not set in environment" unless $ENV{CCM_TEST_DB};

    %test_session = 
    (
	PrintError	=> 0,
	RaiseError	=> 1,
	database	=> $ENV{CCM_TEST_DB},
	UseCoprocess	=> $ENV{CCM_USE_COPROCESS}||0,
    );

    if ($ENV{CCM_TEST_USER})
    {
	# CCM_TEST_USER=user/password@host (Oracle style :)
	@test_session{qw(user password host)} = 
	    $ENV{CCM_TEST_USER} =~ m{^(.*?)/(.*?)\@(.*)};
    }
    else
    {
	die "CCM_TEST_USER not set in environment" 
	    if $^O eq 'MSWin32' || $^O eq 'cygwin';
    }

    # Set the date format (the default is "%c" which depends
    # on the locale, the C library etc and hence makes comparisons
    # with expected values fail)
    # NOTE: restrict yourself to strftime conversion specifiers from
    # the ISO C standard; also, don't use locale dependent conversions
    $ENV{CCM_DATETIME_FMT} = "%Y-%m-%d %H:%M:%S";
}

# all_ok BLOCK AREF [, TEST_NAME]
# check if predicate BLOCK holds for all elements in list
sub all_ok(&$;$)
{
    my ($block, $aref, $test_name) = @_;

    local $Test::Builder::Level = 2;	# report failure for caller of all_ok

    if (my @failed = grep { ! &$block($_) } @$aref)
    {
	fail("all: $test_name");
	diag "\t$test_name failed for:\n";
	diag("\t\t$_\n") foreach @failed;
    }
    else
    {
	pass("all: $test_name");
    }
}

# stringify an array of VCS::CMSynergy::Object's
sub objectnames($)
{
    my $aref = shift;
    return [ map { $_->objectname } @$aref ];
}

# NOTE: We want to prevent Data::Dumper to dump all attributes
# of a VCS::CMSynergy::Object when using the tied hash interface.
sub verbose($$)
{
    return unless defined $ENV{TEST_VERBOSE} && $ENV{TEST_VERBOSE} > 1;

    my ($tag, $result) = @_;
    my $dumper = Data::Dumper->new([ $result], [ $tag ]);
    $dumper->Useqq(1);
    $dumper->Freezer('Freezer');
    print STDERR $dumper->Dump;
}

sub VCS::CMSynergy::Object::Freezer
{
    my $objectname = shift->objectname;
    return bless \$objectname, "VCS::CMSynergy::Object::Dummy";
}

# Shut up annoying Data::Dumper warning
# WARNING(Freezer method call failed): Can't locate object method "Freezer"...
sub VCS::CMSynergy::Freezer	{ return shift; }


### Test::Deep stuff

sub vco		{ return isa("VCS::CMSynergy::Object") & methods(objectname => $_[0]); }

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
