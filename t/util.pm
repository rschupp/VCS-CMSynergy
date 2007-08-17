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
    );

    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
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

sub eqany($$)
{
    my ($a, $b) = @_;

    # only undef equals undef
    return !defined $a unless defined $b;
    return !defined $b unless defined $a;

    # different refs can never be equal
    return 0 if ref $a ne ref $b;

    for (ref $a)
    {
	# compare scalars as strings
	$_ eq "" 	&& return $a eq $b;

	# compare arrays with eqarray
	$_ eq "ARRAY" 	&& return eqarray($a, $b);

	# compare hashes with eqhash
	$_ eq "HASH"	&& return eqhash($a, $b);

	die "eqany: can't handle ref " . ref $a;
    }
}

# eqarray(ARRAYREF, ARRAYREF [, SORTFUNC [, EQFUNC]])
# compare two arrays optionally sorted and values compared 
# by optional equal function (default: eqany)
# NOTE: SORTFUNC as in sort builtin; @_ == 2 means sort using cmp;
# SORTFUNC == undef means no sort
sub eqarray($$;$$)
{
    my ($aref, $bref, $sort, $eq) = @_;
    return 0 unless defined $aref && defined $bref;
    return 0 unless ref $aref eq "ARRAY" && ref $bref eq "ARRAY";

    my (@a, @b);
    if (@_ == 2)
    {
	@a = sort { $a cmp $b } @$aref;
	@b = sort { $a cmp $b } @$bref;
    }
    elsif (!defined $sort)
    {
	@a = @$aref;
	@b = @$bref;
    }
    elsif (ref $sort eq "CODE")
    {
	@a = sort $sort @$aref;
	@b = sort $sort @$bref;
    }
    else
    {
	die "eqarray: don't know how to sort using $sort";
    }
    $eq = \&eqany unless defined $eq;
    while (@a && @b) 
    { 
	return 0 unless &$eq(shift @a, shift @b);
    }
    return @a == 0 && @b == 0;
}

# eqhash(HASHREF, HASHREF [ , KEY => EQFUNC, ... ])
# compare two hashes with optional equal function per key
# (default: eqany)
sub eqhash($$;@)
{
    my ($aref, $bref, %eqkey) = @_;
    return 0 unless defined $aref && defined $bref;
    return 0 unless ref $aref eq "HASH" && ref $bref eq "HASH";
    return 0 unless eqarray([ keys %$aref ], [ keys %$bref ]);
    
    foreach (keys %$aref)
    { 
	my $eq = defined $eqkey{$_} ?  $eqkey{$_} : \&eqany;
	return 0 unless &$eq($aref->{$_}, $bref->{$_});
    }
    return 1;
}

# all BLOCK LIST
# check if predicate BLOCK holds for all elements in list
sub all(&@)
{
    my $pred = shift;
    &$pred($_) || return 0 foreach (@_);
    return 1;
}

sub verbose($$)
{
    return unless $ENV{TEST_VERBOSE};

    my ($tag, $result) = @_;
    print STDERR Data::Dumper->Dump([ $result], [ $tag ]);
}

1;
