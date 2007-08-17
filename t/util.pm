use VCS::CMSynergy;
use Data::Dumper;

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
	UseCoprocess	=> $ENV{CCM_USE_COPROCESS},
    );

    if ($VCS::CMSynergy::Is_MSWin32)
    {
	# CCM_TEST_USER=user/password@host (Oracle style :)
	die "CCM_TEST_USER not set in environment" unless $ENV{CCM_TEST_USER};
	@test_session{qw(user password host)} = 
	    $ENV{CCM_TEST_USER} =~ m{^(.*?)/(.*?)\@(.*)};
    }

    # Set the date format (the default is "%c" which depends
    # on the locale, the C library etc and hence makes comparisons
    # with expected values fail)
    # NOTE: restrict yourself to strftime conversion specifiers from
    # the ISO C standard; also, don't use locale dependent conversions
    $ENV{CCM_DATETIME_FMT} = "%Y-%m-%d %H:%M:%S";

    VCS::CMSynergy->use_ccm_coprocess if $ENV{CCM_TEST_COPROCESS};
}


# all_ok BLOCK AREF [, TEST_NAME]
# check if predicate BLOCK holds for all elements in list
sub all_ok(&$;$)
{
    my ($block, $aref, $test_name) = @_;
    foreach (@$aref)
    {
	next if &$block($_);
	fail("[$_] $test_name");
	return;
    }
    pass($test_name);
}

# stringify an array of VCS::CMSynergy::Object's
sub strobjs($)
{
    my $aref = shift;
    return [ map { "$_" } @$aref ];
}

# NOTE: We want to prevent Data::Dumper to dump all attributes
# of a VCS::CMSynergy::Object when using the tied hash interface.
sub verbose($$)
{
    return unless defined $ENV{TEST_VERBOSE} && $ENV{TEST_VERBOSE} > 1;

    my ($tag, $result) = @_;
    my $dumper = Data::Dumper->new([ $result], [ $tag ]);
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

1;
