use Data::Dumper;
use Test::Deep;

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

# are_vcos AREF
# check that AREF is a reference to an array of VCS::CMSynergy::Objects
sub are_vcos($)
{
    my ($aref) = @_;

    UNIVERSAL::isa($aref, "ARRAY")
    && (grep { UNIVERSAL::isa($_, "VCS::CMSynergy::Object") } @$aref) == @$aref;
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

# check for an array of VCOs (optionally of the given cvtype)
sub vco_array
{
    my ($cvtype) = @_;
    return $cvtype ? array_each(isa("VCS::CMSynergy::Object"),
				methods(cvtype => $cvtype)) 
                   : array_each(isa("VCS::CMSynergy::Object")); 
}

# check whether $got is VCS::CMSynergy::Object with the given objectname
sub vco 	{ Test::Deep::VCO->new(@_); }
sub vco_list	{ map { vco($_) } @_ }

package Test::Deep::VCO;

use Test::Deep::Cmp;

# overload "cmp" to compare objectnames
# NOTE: we need this so that a Test::Deep::VCO can be put into 
# a set() or bag() (whose contents are sorted with "cmp")
use overload 
    cmp		=> sub { $_[0]->{val} cmp $_[1]->{val} },
    '""'	=> sub { $_[0]->{val} };         

sub init
{
	my $self = shift;

	my $val = shift;
	$self->{val} = $val;
}

sub descend
{
	my $self = shift;
	my $got = shift;

	return UNIVERSAL::isa($got, "VCS::CMSynergy::Object") 
	       && $got->objectname eq $self->{val};
}

sub diag_message
{
	my $self = shift;
	my $where = shift;

	return "Checking $where whether is VCS::CMSynergy::Object \"$self->{val}\"";
}

1;
