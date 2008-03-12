package VCS::CMSynergy;

# $Revision$

our $VERSION = '1.33';

use 5.006_000;				# i.e. v5.6.0
use strict;

use VCS::CMSynergy::Client qw(
    is_win32 $Debug $Error $Ccm_command
    _exitstatus _error _usage);
our @ISA = qw(VCS::CMSynergy::Client);


use Carp;
use Config;
use File::Spec;
use File::Temp qw(tempfile);		# in Perl core v5.6.1 and later

use constant ROW_ARRAY	=> 0;
use constant ROW_HASH	=> 1;
use constant ROW_OBJECT	=> 2;

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	eval "use Filesys::CygwinPaths qw(:all); 1" or die $@;
    }
}


sub import
{
    my $class = shift;

    my %use =
    (
	tied_objects		=> 0,
	cached_attributes	=> 0,
    );

    foreach (@_)
    {
	my $opt;
	die qq[Invalid option "$_" in "use ].__PACKAGE__.qq["]
	    unless ($opt) = /^[!:](.*)$/i and exists $use{$opt};
	$use{$opt} = /^:/ ? 1 : 0;
    }

    while (my ($opt, $value) = each %use)
    {
	eval "use constant use_$opt => $value";
    }

    # require V::C::Object _after_ use_* have been defined,
    # so that optimization based on constant expressions can 
    # e.g. eliminate branches guarded with "if (V::C::use_cached_attributes)"
    require VCS::CMSynergy::Object;
    require VCS::CMSynergy::ObjectTieHash if use_tied_objects();
}

my %start_opts =
(
    KeepSession		=> undef,
    UseCoprocess	=> undef,
    CCM_ADDR		=> undef,
    ini_file		=> undef,
    remote_client	=> undef,
    database		=> "-d",
    home		=> "-home",
    host		=> "-h",
    password		=> "-pw",
    role		=> "-r",
    ui_database_dir	=> "-u",
    user		=> "-n",
);

sub new
{
    my ($class, %args) = @_;

    my %client_args;
    foreach (keys %args)
    {
	$client_args{$_} = delete $args{$_} 
	    if exists $VCS::CMSynergy::Client::opts{$_};
    }
    return $class->_start(VCS::CMSynergy::Client->new(%client_args), %args);
}


sub _start
{
    my ($class, $client, %args) = @_;
    croak(__PACKAGE__."::_start: $client is not a VCS::CMSynergy::Client")
	unless UNIVERSAL::isa($client, 'VCS::CMSynergy::Client');

    # make a deep clone of $client 
    my $self = { %$client };
    $self->{env} = { %{ $client->{env} } } if $client->{env};
    bless $self, $class;

    # Cygwin: some start options denote path names that are 
    # passed down to Synergy; convert them to native Windows form
    if ($^O eq 'cygwin')
    {
	foreach (qw/home ini_file ui_database_dir/)
	{
	    $args{$_} = fullwin32path($args{$_}) if defined $args{$_};
	}
    }

    my @start = qw/start -m -q -nogui/;
    while (my ($arg, $value) = each %args)
    {
	croak(__PACKAGE__.q[::object: unrecognized argument "$arg"]) 
	    unless exists $start_opts{$arg};

	$self->{$arg} = $value unless $arg eq "password";
	push @start, $start_opts{$arg} => $value if defined $start_opts{$arg};
    }

    $self->{env}->{CCM_ADDR} = delete $self->{CCM_ADDR} if defined $self->{CCM_ADDR};
    push @start, '-rc' if $self->{remote_client};

    if (defined $self->ccm_addr)
    {
	$self->{KeepSession} = 1 unless defined $self->{KeepSession};
	if ($Debug)
	{
	    my $ccm_addr = $self->ccm_addr;
	    $self->trace_msg($self->{KeepSession} ? 
		qq[will keep session "$ccm_addr"\n] :
		qq[will not keep session "$ccm_addr"\n]);
	}

	if (is_win32)
	{
	    # figure out user of session specified by CCM_ADDR
	    $self->{user} = 
		$self->ps(rfc_address => $self->ccm_addr)->[0]->{user};

	    # create a minimal ini file (see below for an explanation)
	    (my $inifh, $self->{ini_file}) = tempfile(SUFFIX => ".ini", UNLINK => 0);
	    $self->{ini_file} = fullwin32path($self->{ini_file}) if $^O eq 'cygwin';
	    			# because this name is passed down to ccm.exe
		
	    print $inifh "[UNIX information]\nUser = $self->{user}\n";
	    close($inifh);
	    push @{ $self->{files_to_unlink} }, $self->{ini_file};
	}
    }
    else
    {
	# NOTE: If neither database nor CCM_ADDR was specified "ccm start ..."
	# will fail later on, but with rather cryptic messages from CM Synergy;
	# hence better fail early
	return $self->set_error("don't know how to connect to CM Synergy: neither database nor CCM_ADDR specified")
	    unless $args{database};

	unless (defined $self->{ini_file})
	{
	    if (is_win32)
	    {
		# NOTES: 
		# (1) "ccm start -f nul ..." doesn't work on Windows
		#     (leads to error from ccm_seng), 
		#     so use an empty ini_file instead
		# (2) we can't use UNLINK=>1 with tempfile, because 
		#     the actual unlink may occur before the session is
		#     stopped and Windows refuses removing the "busy" file
		(undef, $self->{ini_file}) = tempfile(SUFFIX => ".ini", UNLINK => 0);
		$self->{ini_file} = fullwin32path($self->{ini_file}) if $^O eq 'cygwin';
		push @{ $self->{files_to_unlink} }, $self->{ini_file};
	    }
	    else
	    {
		$self->{ini_file} = File::Spec->devnull;
	    }
	}
	push @start, "-f", $self->{ini_file};

	my ($rc, $out, $err) = $self->_ccm(@start);
	return $self->set_error($err || $out) unless $rc == 0;

	$self->{env}->{CCM_ADDR} = $out;
	$Debug && $self->trace_msg(qq[started session "$out"\n]);
    }

    # NOTE: Use of $CCM_INI_FILE fixes the annoying `Warning:
    # Security violation.  User JLUSER is not authorized to the
    # Continuus interface at ...'  when running on Windows.
    #
    # Background: The problem is the obsolete ccm.ini file in
    # Windows' %SystemRoot%.  If ccm_gui or "ccm start ..." is
    # invoked _without_ specifying an ini file it writes the
    # Unix user (as given in the login popup or -n option, resp.)
    # into this file. If $CCM_INI_FILE is not set, all other "ccm ..."
    # invocations will read this file and check its "user"
    # entry against the session identified by $CCM_ADDR. If
    # they don't match, the above warning is issued and the
    # command aborted.  If we already have have an ini_file we
    # just set $CCM_INI_FILE to its name. Otherwise we fake
    # a minimal ini file with the correct setting of "user"
    # and set $CCM_INI_FILE to its name.
    #
    # NOTE: CM Synergy versions >= 6.0 on Windows do not use 
    # %SystemRoot%\ccm.ini any more. However, the problem persists:
    # if there's a [UNIX information] section in $CCM_HOME\etc\ccm.ini
    # or the user's personal ccm.ini its "User" setting will be used
    # and may trigger the "security violation".

    $self->{env}->{CCM_INI_FILE} = $self->{ini_file} if is_win32;

    # remember the process that created $self (so we can check in DESTROY)
    $self->{pid} = $$;

    if ($self->{UseCoprocess})
    {
	if ($self->_spawn_coprocess)
	{
	    $Debug && $self->trace_msg("spawned coprocess (pid=".$self->{coprocess}->pid.")\n", 8);
	}
	else
	{
	    carp(__PACKAGE__." new: can't establish coprocess: $self->{error}\n" .
	         "-- ignoring UseCoprocess");
	}
    }

    # cache some info from database; this also doubles as a test for a valid session
    {
	my ($rc, $out, $err) = $self->_ccm(qw/delimiter/);
	return $self->set_error($err || $out) unless $rc == 0;
	$self->{delimiter} = $out;

	$self->{objectname_rx} = 
	    qr/^(.*?)\Q$self->{delimiter}\E(.*?):(.*?):(.*?)$/;
					# -> (name, version, cvtype, instance)
	$self->{finduse_rx} = 
	    qr/^\t(.*?)\Q$self->{delimiter}\E.*?\@(.*?)$/;
    					# -> (path, project)
    }

    # NOTE: If option `database' was present it may not be in the
    # canonical form; purge it from $self so that $self->database
    # will recompute it on demand.
    delete $self->{database};

    $self->{objects} = {} if use_cached_attributes();

    if ($Debug >= 9)
    {
	require Data::Dumper;
	local $Data::Dumper::Useqq = 1;
	$self->trace_msg(Data::Dumper->Dump([$self], ["$self"]));
    }

    return $self;
}


sub DESTROY 
{
    my $self = shift;

    # no-op if the session has not yet been established
    return unless $self->ccm_addr;	

    # no-op if this is not the process that created $self
    return unless $self->{pid} == $$;	

    # NOTE: DESTROY might be called implicitly while unwinding 
    # stack frames during exception processing, e.g.
    #
    # eval {
    #   my $ccm = VCS::CMSynergy->new(...);
    #   ...
    #   die "D.O.A."			# <-- exception thrown
    #   ...
    # };
    # print "oops: $@\n" if $@;		# <-- handle it
    #
    # The exception causes a premature exit from the eval block.
    # But this block is also the scope of $ccm, hence $ccm->DESTROY
    # is called. Any eval block encountered during processing of DESTROY()
    # will reset $@  - even if no excpetion is thrown. Hence $@
    # might be empty at "print...". 
    # We localize $@ to avoid this unexpected behaviour.
    # FIXME: might be more correct to push localization into the
    # offending methods.
    local $@;

    local $?;				# don't screw up global $?
    $self->_kill_coprocess if $self->{coprocess};

    # don't stop session if KeepSession is set 
    unless ($self->{KeepSession})
    {
	$self->_ccm(qw/stop/);
	$Debug && $self->trace_msg("stopped session ".$self->ccm_addr."\n");
    }

    # on Windows, certain files (e.g. the fake ccm.ini) might still be busy
    my @files_to_unlink;
    foreach (@{ $self->{files_to_unlink} })
    {
	unlink($_) or push @files_to_unlink, $_;
    }
    if (is_win32 && @files_to_unlink)
    {
        # wait a little, then try again
	sleep(2);
	unlink(@files_to_unlink);
    }

    %$self = ();			# paranoia setting
}


sub ccm_addr	{ return shift->{env}->{CCM_ADDR}; }

sub delimiter	{ return shift->{delimiter}; }


sub _my_ps
{ 
    my ($self, $field) = @_;

    my $ccm_addr = $self->ccm_addr;
    my $ps = $self->ps(rfc_address => $ccm_addr);
    return $self->set_error("can't find current session `$ccm_addr' in `ccm ps'") 
	unless $ps && @$ps > 0;
    return $ps->[0]->{$field};
}

# determine database path (in canonical format) etc from `ccm ps'
__PACKAGE__->_memoize_method(database => sub { shift->_my_ps('database'); });
__PACKAGE__->_memoize_method(user => sub { shift->_my_ps('user'); });


sub query
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(qw/query -u/, @_);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr

    return [ split(/\n/, $out) ] if $rc == 0;
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out);
}


sub query_arrayref
{
    my $self = shift;
    _usage(@_, 1, undef, '$query, @keywords');

    my $query = shift;
    return $self->_query($query, \@_, ROW_ARRAY);
}


sub query_hashref
{
    my $self = shift;
    _usage(@_, 1, undef, '$query, @keywords');

    my $query = shift;
    return $self->_query($query, \@_, ROW_HASH);
}


sub query_object
{
    my $self = shift;
    _usage(@_, 1, undef, '$query, @attributes');

    my $query = shift;
    return $self->_query($query, \@_, ROW_OBJECT);
}

*query_object_with_attributes = \&query_object;		# compatibility alias

sub query_count
{
    my $self = shift;
    _usage(@_, 1, 1, '$query');

    my $query = shift;
    my ($rc, $out, $err) = $self->_ccm(
	qw/query -u -ns -nf -format X/, $self->_expand_query($query));

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return 0 if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $out =~ tr/X/X/ if $rc == 0;			# count 'em X's
    return $self->set_error($err || $out);
}


# NOTE: We use \cA and \cD as record/field separators.
# SYNERGY/Change uses \x1C-\x1E in attribute
# "transition_log" of "problem" objects, so these are out.
# Also people have been known to enter strange characters
# like \cG even when using a GUI exclusively.
# Change these at your own risk, YMMV.

our $RS = "\cA";	# record separator for query etc
our $FS = "\cD";	# field separator for query etc

# helper method: query with correct handling of multi-line attributes
sub _query
{
    my ($self, $query, $keywords, $row_type) = @_;

    $query = $self->_expand_query($query);

    my $want = _want($row_type == ROW_OBJECT, $keywords);

    my $want_finduse = delete $want->{finduse};
    croak(__PACKAGE__.qq[::_query: keyword "finduse" not allowed when ROW_OBJECT wanted])
	if $want_finduse && $row_type == ROW_OBJECT;

    my $format = $RS . join($FS, values %$want) . $FS;

    my ($rc, $out, $err) = $want_finduse ?
	$self->_ccm_with_option(
	    Object_format => $format, 
	    qw/finduse -query/ => $query) :
	$self->_ccm( 
	    qw/query -u -ns -nf -format/ => $format, $query);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) && $out eq "" && $err eq "";

    # NOTE: if the query string contained a syntax error, Synergy
    # prints "Syntax error in query request", but won't tell you the
    # query string, making it hard to diagnose the problem.
    # So append the query string to the error message.
    return $self->set_error(($err || $out).qq[\n  Query was "$query"]) 
	unless $rc == 0;

    my @result;
    foreach (split(/\Q$RS\E/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\Q$FS\E/, $_, -1);	# don't strip empty trailing fields
	my %finduse;

	if ($want_finduse)
	{
	    # finduse information is the last "column" 
	    my $fu_lines = pop @cols;	

	    # finduse lines are of the forms
	    #
	    #   \t relative_path/name-version@pname-pversion 
	    #   \t relative_path/name-version@pname-pversion:project:pinstance
	    #
	    # which we parse into a hash
	    #   "project_objectname" => "relative_path/name"

	    # NOTE: Starting with CCM 6.3, project objects may have instances
	    # other than '1' (either for DCM reasons, or because someone
	    # created a second project with the same name while the
	    # model attribute "multiple_local_proj_instances" was TRUE).
	    # CCM 6.3 apparently still returns "proj_vers" if instance='1' and
	    # the full objectname otherwise. We return the full objectname
	    # in any case.

	    unless ($fu_lines =~ /Object is not used in scope/)
	    {
		foreach (split(/\n/, $fu_lines))
		{
		    next if /^\s*$/;
		    my ($path, $project) = /$self->{finduse_rx}/
			or return $self->set_error(
			    qq[unrecognizable line returned from "finduse -query": "$_"]);
		    $finduse{$self->_projspec2objectname($project)} = $path;
		}
	    }
	}

	my $row = $self->_parse_query_result($want, \@cols, $row_type == ROW_OBJECT);
	$row->{finduse} = \%finduse if $want_finduse;
	push @result, $row;
    }

    if ($row_type == ROW_ARRAY)
    {
	$_ = [ @$_{@$keywords} ] foreach @result;
    }
    return \@result;
}

# Sigh. "ccm query -f %objectname" returns old-style fullnames
# (i.e. "instance/cvtype/name/version") for certain legacy types of 
# objects, e.g. "cvtype" and "attype". But CM Synergy
# doesn't accept these where a "file_spec" is expected 
# (at least on Unix, because they contain slashes). 
# Hence rewrite these fullnames to objectnames.
# Arrggh. Some moron implemented a bogus "objectname" acc_method 
# for release objects (i.e. type "releasedef") that
# emits ":" as the first separator. These objectnames aren't
# accepted as file_specs either. Rewrite them, too.
sub _fullname2objectname
{
    my ($self, $fullname) = @_;
    $fullname =~ s{^(.*?)/(.*?)/(.*?)/(.*?)$}
	          {$3$self->{delimiter}$4:$2:$1};
    $fullname =~ s{^(.*?):(.*?):releasedef:(.*?)$}
	          {$1$self->{delimiter}$2:releasedef:$3};
    return $fullname;
}

# NOTE: The Synergy pseudo attributes (e.g. %task) are implemented in
# baselib/src/base/pseudo_attrs.ac (except for the hard-wired %objectname
# and %displayname) and the table in attribute "pseudo_attrs" 
# of base-1:model:base.

# NOTE: out methods are never called with $_[1] undef
my %_rewrite_rule = 
(
    objectname => 
    {
	in		=> "%objectname",
	out		=> sub { $_[0]->_fullname2objectname($_[1]); },
	row_object_ok	=> 1,
    },
    object => 
    {
	in		=> "%objectname",
	out		=> sub { $_[0]->object($_[0]->_fullname2objectname($_[1])); },
	row_object_ok	=> 1,
    },
    task_objects => 
    {
	in		=> "%task",
	out		=> sub { [ map { $_[0]->task_object($_) } split(/,/, $_[1]) ]; },
	row_object_ok	=> 0,
    },
    cr_objects =>
    {
	in		=> "%change_request",
	out		=> sub { [ map { $_[0]->cr_object($_) } split(/,/, $_[1]) ]; },
	row_object_ok	=> 0,
    },
    baseline_project =>
    {
	in		=> "%baseline",
	out		=> sub { $_[0]->project_object($_[0]); },
	row_object_ok	=> 0,
    },
    baseline_object =>
    {
	in		=> "%in_baseline",
	out		=> sub { $_[0]->baseline_object($_[1]); },
	row_object_ok	=> 0,
    },
);


# helper (not a method): build "want" array from keyword list (common case)
# NOTE: if $want_row_object is true, key "object" will be automatically
#       added to the returned hash
sub _want
{
    my ($want_row_object, $keywords) = @_;
    my %want = map { $_ => "%$_" } @$keywords;
    $want{object} = "%objectname" if $want_row_object;

    # handle special keywords
    foreach (keys %want)
    {
	if (my $rule = $_rewrite_rule{$_})
	{
	    croak(__PACKAGE__.qq[::_want: keyword "$_" not allowed when ROW_OBJECT wanted]) 
		if $want_row_object && !$rule->{row_object_ok};
	    $want{$_} = $rule->{in};
	}
    }

    return \%want;
}

sub _parse_query_result
{
    my ($self, $want, $cols, $want_row_object) = @_;

    my %row;
    
    # strip trailing newline (for consistency with get_attribute()),
    # translate "<void>" to undef and fill into correct slots;
    # @$cols are in the same order as keys %$want
    @row{keys %$want} = map { s/\n\z//; /^<void>$/ ? undef : $_ } @$cols;
    
    # handle special keywords
    foreach (keys %$want)
    {
	if ($_rewrite_rule{$_} && defined $row{$_})
	{
	    $row{$_} = $_rewrite_rule{$_}->{out}->($self, $row{$_});
	}
    }

    if ($want_row_object)
    {
	my $obj = delete $row{object};
	$obj->_update_acache(\%row);
	return $obj;
    }
    return \%row;
}

# helper
sub _expand_query
{
    my ($self, $query) = @_;
    if (ref $query eq 'HASH')
    {
	$query = $self->_query_shortcut($query);
    }
    else
    {
	# Sanitize query string by replacing whitespace (esp. newlines)
	# by a single blank except inside single or double quotes.
	# This helps to improve the legibility of longish queries with 
	# whitespace and line breaks (which CM Synergy's CLI dosen't grok).
	$query =~ s/('.*?'|".*?"|[^'"\s]+)|(\s+)/defined $2 ? " " : $1/sge;
    }
    return $query;
}

my %ac_cvtype = map { $_ => "AC/cvtype/$_/1" } 
                    qw/ admin asm attype bstype cvtype mcomp model pdtype /;

# helper: expand shortcut queries
sub _query_shortcut
{
    my ($self, $hashref) = @_;

    $Debug >= 5 && $self->trace_msg(
	"shortcut query { ".join(", ", map { "$_ => $hashref->{$_}" } keys %$hashref)." }\n", 5);

    my @clauses;
    while (my ($key, $value) = each %$hashref)
    {
	my $ref = ref $value;
	if ($ref eq '')
	{
	    for ($key)
	    {
		/^task$/ && do 		# same as "ccm query -task ..."
		{
		    push @clauses, "is_associated_cv_of(task('$value'))";
		    next;
		};
		/^match$/ && do
		{
		    push @clauses, "name match '$value'";
		    next;
		};
		/^(cv)?type$/ && do
		{
		    # rumor (D. Honey) has it that 
		    # "has_cvtype('base/cvtype/foo/1')" is somehow faster
		    # than "type='foo'; note that the two are not synonyms,
		    # since the latter also applies to the AC cvtypes 
		    # like "admin" or "model"
		    my $cvtype = $ac_cvtype{$value} || "base/cvtype/$value/1";
		    push @clauses, "has_cvtype('$cvtype')";
		    next;
		};
		push @clauses, "$key="._quote_value($value);
	    }
	}
	elsif ($ref eq 'ARRAY')
	{
	    my $args = join(",", map { _quote_value($_) } @$value);
	    push @clauses, "$key($args)";
	}
	elsif ($ref eq 'HASH')
	{
	    my $nested = $self->_query_shortcut($value);
	    push @clauses, "$key($nested)";
	}
	else
	{
	    (my $method = (caller(1))[3]) =~ s/^.*:://;
	    croak(qq[$method: dunno how to handle "$key => $ref" in shortcut query]);
	}
    }

    my $result = join(" and ", @clauses);
    $Debug >= 5 && $self->trace_msg("shortcut query => $result\n", 5);
    return $result;
}

# helper (not a method): smart quoting of string or boolean values
# NOTE: CM Synergy seems to use the following quoting rules
# for the right hand side of an "attribute value clause" in a query:
# - string and text values must be quoted
# - boolean values ("TRUE" or "FALSE") must not be quoted
# - integer values must not be quoted, but must always have a leading sign
# - time values must be written as "time('Fri Dec 12 1997')"
sub _quote_value
{
    local ($_) = @_;
    return /^(TRUE|FALSE)$/ ? $_ : # don't quote boolean
	   /'/ ? qq["$_"] :	   # use double quotes if contains single quote
	   qq['$_'];		   # use single quotes otherwise
}


sub history
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(history => @_);
    return $self->set_error($err || $out) unless $rc == 0;

    return [ split(/^\*+\n?/m, $out) ];
}


sub history_arrayref
{
    my $self = shift;
    _usage(@_, 1, undef, '$file_spec, @keywords');

    my $file_spec = shift;
    return $self->_history($file_spec, \@_, ROW_ARRAY);
}


sub history_hashref
{
    my $self = shift;
    _usage(@_, 1, undef, '$file_spec, @keywords');

    my $file_spec = shift;
    return $self->_history($file_spec, \@_, ROW_HASH);
}

# helper: history with correct handling of multi-line attributes
sub _history
{
    my ($self, $file_spec, $keywords, $row_type) = @_;

    my $want = _want($row_type == ROW_OBJECT, $keywords);
    my $want_predecessors = delete $want->{predecessors};
    my $want_successors = delete $want->{successors};
    croak(__PACKAGE__.qq[::_history: keyword "predecessors" or "successors" not allowed when ROW_OBJECT wanted]) 
	if ($want_predecessors || $want_successors) && $row_type == ROW_OBJECT;

    my $format = $RS . join($FS, values %$want) . $FS;

    # NOTE: documentation says option "-format" is allowed,
    # but implementation accepts only "-f"
    my ($rc, $out, $err) = $self->_ccm(qw/history -f/, $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\Q$RS\E/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\Q$FS\E/, $_, -1);	# don't strip empty trailing fields
	
	# history information is the last "column"
	my $history = pop @cols;

	my $row = $self->_parse_query_result($want, \@cols, 0);

	if ($want_predecessors || $want_successors)
	{
	    # parse history information
	    my ($predecessors, $successors) = $history =~
		/^Predecessors:\n\t?(.*)
		 ^Successors:\n\t?(.*)
		 ^\*
		/msx;

	    if ($want_predecessors)
	    {
		$row->{predecessors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $predecessors) ];
	    }
	    if ($want_successors)
	    {
		$row->{successors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $successors) ];
	    }
	}

	push @result, $row;
    }

    if ($row_type == ROW_ARRAY)
    {
	$_ = [ @$_{@$keywords} ] foreach @result;
    }
    return \@result;
}


sub finduse
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(finduse => @_);

    # NOTE: `ccm finduse ...' without `-query' complains if some of 
    # the given objects do not exist (and exits with status 1 unless at least
    # one exists). But for `ccm finduse -query ...', if there are no hits, 
    # the command exits with status 1 and produces no output on either 
    # stdout and stderr. (This is the same behaviour as for `ccm query ...'.) 
    # We will not produce an error in any case. However, the returned array
    # may contain fewer elements than the number of file_spec arguments.

    if ($rc == 0)
    {
	my (@result, $uses);
	foreach (split(/\n/, $out))
	{
	    # ignore complaints about non-existing objects 
	    # and the dummy "use" line printed if object is not used anywhere

	    next if /Object version could not be identified|Object is not used in scope/;

	    # a usage line is matched by finduse_rx
	    if (/$self->{finduse_rx}/)
	    {
		my ($path, $project) = ($1, $2);
		$uses->{$self->_projspec2objectname($project)} = $path;
		next;
	    }

	    # otherwise the line describes an object satisfying the query
	    # in the format given by option `Object_format' (default:
	    # "%displayname %status %owner %type %project %instance %task")
	    push(@result, [ $_, $uses = {} ]);
	}
	return \@result;
    }
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out);
}


sub findpath
{
    my ($self, $file_spec, $proj_vers) = @_;
    my $finduse = $self->finduse($file_spec);
    return unless defined $finduse;
    return $self->set_error("`$file_spec' matches more than one object") 
	unless @$finduse == 1;
    return $finduse->[0]->[1]->{$proj_vers};
}


sub relations_hashref
{
    my ($self, %args) = @_;

    my %defaulted;
    foreach my $arg (qw/from_attributes to_attributes/)
    {
	croak(__PACKAGE__.qq[::relations_hashref: optional argument "$arg" must be an array ref])
	    if exists $args{$arg} && !UNIVERSAL::isa($args{$arg}, 'ARRAY');

	# default keyword "objectname"
	unless ($args{$arg})
	{
	    $args{$arg} = [ qw/objectname/ ];
	    $defaulted{$arg}++;
	}
    }

    my $result = $self->_relations(\%args, 0);
    return unless $result;

    # if we defaulted "objectname" above, replace the corresponding
    # hash containing the sole key "objectname" with its value
    foreach my $arg (qw/from to/)
    {
	if ($defaulted{"${arg}_attributes"})
	{
	    $_->{$arg} = $_->{$arg}->{objectname} foreach @$result;
	}
    }
    return $result;
}


sub relations_object
{
    my ($self, %args) = @_;

    foreach my $arg (qw/from_attributes to_attributes/)
    {
	croak(__PACKAGE__.qq[::relations_object: optional argument "$arg" must be an array ref])
	    if $args{$arg} && !UNIVERSAL::isa($args{$arg}, 'ARRAY');
	$args{$arg} ||= [];		# _relations below likes 'em defined
    }

    return $self->_relations(\%args, 1);
}


# helper method: synthesize command and parse result of "ccm relate -show ..."
sub _relations
{
    my ($self, $args, $want_row_object) = @_;
    # NOTE: $args->{from_attributes}/$args->{to_attributes} must not be undef

    my $want_from = _want($want_row_object, $args->{from_attributes});
    my $ncol_from = keys %$want_from;
    my $want_to = _want($want_row_object, $args->{to_attributes});
    my $ncol_to = keys %$want_to;

    # NOTE: If the "from" part (the part before "::") of the format
    # or the "to" part are empty, Synergy may default it from
    # the other part. Hence both "from" and "to" part below are never
    # empty, even if $want_from or $want_to are empty.
    my $format = 
	$RS . 			# record delimiter
	join($FS, 			# column separator
	    values %$want_from,		# "from" part
	    "::", 			# will be replaced by name of relation
	    values %$want_to) .		# "to" part
	$FS;				# will be followed by create_time

    my ($rc, $out, $err) = $self->_ccm(
	qw/relate -show -format/ => $format, 
	    map { defined $args->{$_} ? ( "-$_" => $args->{$_}) : () } 
		qw/from to name/);

    # NOTE: if there are no hits, `ccm relate -show' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;

    my (@result, $from, $to);
    foreach (split(/\Q$RS\E/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\Q$FS\E/, $_, -1);	# don't strip empty trailing fields

	# first $ncol_from columns are the "from" part;
	# avoid to parse "from" part more than once # if "from => ..." was specified
	my @cols_from = splice @cols, 0, $ncol_from;
	$from = $self->_parse_query_result($want_from, \@cols_from, $want_row_object)
	    unless $args->{from} && $from;

	# next column is the name of the relation; trim whitespace
	(my $name = shift @cols) =~ s/^\s+|\s+$//g;	

	# next $ncol_to columns are the "to" part;
	# avoid to parse "to" part more than once if "to => ..." was specified
	my @cols_to = splice @cols, 0, $ncol_to;
	$to = $self->_parse_query_result($want_to, \@cols_to, $want_row_object)
	    unless $args->{to} && $to;

	# last column is the create_time of the relation; trim whitespace
	(my $create_time = shift @cols) =~ s/^\s+|\s+$//g;

	push @result, 
	    {
		from		=> $from,
		to		=> $to,
		name		=> $name,
		create_time	=> $create_time,
	    };
    }
    return \@result;
}



sub project_tree
{
    my $self = shift;
    _usage(@_, 1, undef, '\\%options, @projects');

    my ($options, @projects) = @_;
    $options ||= {};
    croak(__PACKAGE__.qq[::project_tree: argument 1 ("options") must be a HASH ref: $options])
	unless ref $options eq "HASH";
    $options->{attributes} ||= [];
    my $mark_projects = delete $options->{mark_projects};
    # NOTE: $options->{attributes} will be checked by traverse() below

    my %tree;
    my $tag = 0;
    foreach my $proj (
	map { ref $_ ?  $_ : $self->project_object($_) } @projects)
    {
	$proj->traverse(
	    { 
		subprojects	=> $options->{subprojects},
		attributes	=> $options->{attributes},
		wanted		=> sub
		{
		    # skip projects unless "mark_projects" is in effect
		    return if $_->is_project && !$mark_projects;

		    # store into %tree with relative workarea pathname as the key
		    # NOTE: VCS::CMSynergy::Traversal::path() has the same
		    # value when invoked for a project and its top level
		    # directory; the "||=" below makes sure we dont't overwrite
		    # the project entry when "mark_projects" is in effect
		    my $path = VCS::CMSynergy::Traversal::path($options->{pathsep});
		    @projects == 1 ? $tree{$path} : $tree{$path}->[$tag] ||= $_;
		},
	    }) or return;
	$tag++;
    }

    return \%tree;
}


sub get_attribute
{
    my $self = shift;
    _usage(@_, 2, 2, '$attribute_name, $file_spec');

    my ($name, $file_spec) = @_;
    my ($rc, $out, $err) = $self->_ccm(qw/attribute -show/, $name, $file_spec);
    return $out if $rc == 0;
    return if ($err || $out) =~ /Attribute .* does not exist on object/;
    return $self->set_error($err || $out);
}


sub set_attribute
{
    my $self = shift;
    _usage(@_, 3, 3, '$attribute_name, $file_spec, $value');

    my ($name, $file_spec, $value) = @_;

    # try "ccm attribute -modify ..." first
    my ($rc, $out, $err) = $self->_ccm_attribute(
	-modify => $name, -value => $value, $file_spec);

    # if this fails because the attribute is inherited,
    # try "ccm attribute -force -create ..."
    if ($rc != 0 && ($err || $out) =~ /Attribute .* is inherited/)
    {
	# determine attribute's type 
	my $type = $self->list_attributes($file_spec)->{$name}
	    or return $self->set_error(
		"oops: attribute $name on `$file_spec' seems inherited, but doesn't show with `ccm attr -la'");
	
	($rc, $out, $err) = $self->_ccm_attribute(
	    -create => $name, -value => $value, -type => $type, -force => $file_spec);
    }

    return $value if $rc == 0;
    return $self->set_error($err || $out);
}


# helper method (used for "ccm attr -modify" and "ccm attr -force -create")
sub _ccm_attribute
{
    my ($self, @args) = @_;	# @args must contain ..., -value => $value, ...

    # squeeze -value => $value from @args
    my $value;
    for (my $i = 0; $i < @args; $i++)
    {
	next unless $args[$i] =~ /^-(?:v|value)$/;
	(undef, $value) = splice @args, $i, 2;
	last;
    }
    croak(__PACKAGE__.qq[::_ccm_attribute: mssing argument "-value"])
	unless defined $value;

    if ($value eq "")
    {
	# Setting a text attribute to an empty string is a real PITA:
	# - CM Synergy will launch text_editor, even if "-v ''" was specified
	# - if the temporary file containing the attribute's value is empty 
	#   after the editor exits, CM Synergy prompts with:
	#	Result of edit is an empty attribute.
	#	Confirm: (y/n) [n] 
	
	# the following doesn't work on Windows (CCM seems to read 
	# the confirmation answer directly from CON:, _not_ from stdin)
	croak(__PACKAGE__."::_ccm_attribute: setting a text attribute to an empty string is not supported on Windows")
	    if is_win32;

	return $self->_ccm_with_option(
	    text_editor => $^O eq 'MSWin32' ?
		qq[cmd /c echo off > %filename ] :  	#/
		qq[$Config{cp} /dev/null %filename],
	    attribute => @args, 
	    { in =>  \"y\n" });
    }

    if (($self->{coprocess} && (length($value) > 1600 || $value =~ /["\r\n]/))
        || (is_win32 && (length($value) > 100 || $value =~ /[%<>&"\r\n]/)))
    {
	# Use ye olde text_editor trick if $value may cause problems
	# (depending on execution mode and platform) because its
	# too long or contains unquotable characters or...
	return $self->ccm_with_text_editor($value, attribute => @args);
    }

    return $self->_ccm(attribute => @args, -value => $value);
}


sub create_attribute
{
    my $self = shift;
    _usage(@_, 3, undef, '$name, $type, $value, @file_specs');

    my ($name, $type, $value, @file_specs) = @_;
    croak(__PACKAGE__.'::create_attribute: argument 3 (value) must be defined')
	unless defined $value;

    my ($rc, $out, $err) = $self->_ccm_attribute(
	    -create => $name, -value => $value, -type => $type, @file_specs);
    return $self->set_error($err || $out) unless $rc == 0;
    return 1;
}


sub delete_attribute
{
    my $self = shift;
    _usage(@_, 1, undef, '$name, @file_specs');

    my ($name, @file_specs) = @_;
    return scalar $self->ccm(qw/attribute -delete/, $name, @file_specs);
}


sub copy_attribute
{
    my $self = shift;
    _usage(@_, 3, undef, '{ $name | \\@names }, [ \\@flags, ] $from_file_spec, $to_file_spec...');

    my ($name, @file_specs) = @_;
    $name = join(':', @$name) if UNIVERSAL::isa($name, 'ARRAY');

    my @flags = UNIVERSAL::isa($file_specs[0], 'ARRAY') ?
	map { "-$_" } @{ shift @file_specs } : ();
    
    return scalar $self->ccm(qw/attribute -copy/, $name, @flags, @file_specs);
}


sub list_attributes
{
    my $self = shift;
    _usage(@_, 1, 1, '$file_spec');

    my $file_spec = shift;
    my ($rc, $out, $err) = $self->_ccm(qw/attribute -la/, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my %attrs = $out =~ /^(\S+) \s* \( (.*?) \)/gmx;
    return \%attrs;
}


sub property
{
    my $self = shift;
    _usage(@_, 2, 2, '{ $keyword | \@keywords }, $file_spec');

    my ($keyword_s, $file_spec) = @_;
    if (UNIVERSAL::isa($keyword_s, 'ARRAY'))
    {
	return $self->_property($file_spec, $keyword_s, 0);
    }
    else
    {
	my $row = $self->_property($file_spec, [ $keyword_s ], 0) or return;
	return $row->{$keyword_s};
    }
}


sub _property
{
    my ($self, $file_spec, $keywords, $want_row_object) = @_;

    my $want = _want($want_row_object, $keywords);
    my $format = $RS . join($FS, values %$want) . $FS;

    my ($rc, $out, $err) = 
	$self->_ccm(qw/properties -nf -format/, $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my (undef, $props) = split(/\Q$RS\E/, $out, -1);
    my @cols = split(/\Q$FS\E/, $props, -1);	# don't strip empty trailing fields
    return $self->_parse_query_result($want, \@cols, $want_row_object);
}


sub cat_object
{
    my $self = shift;
    _usage(@_, 1, 2, '$object [, $destination]');

    my $want_return = @_ == 1;
    my ($object, $destination) = @_;
    croak(__PACKAGE__.qq[::cat_object: argument 1 (object) must be a VCS::CMSynergy::Object: $object])
	unless UNIVERSAL::isa($object, "VCS::CMSynergy::Object");

    # [DEPRECATE < 6.3]
    return $self->_cat_binary(@_)
	if $self->version < 6.3 && $self->_attype_is_binary($object->cvtype);

    my $out;
    $destination = \$out if $want_return;

    my ($rc, undef, $err) = $self->_ccm(
	cat => $object, { out => $destination, binmode_stdout => 1 });

    return $self->set_error($err || "`ccm cat $object' failed") unless $rc == 0;
    return $want_return ? $out : 1;
}

# [DEPRECATE < 6.3]
sub _cat_binary
{
    my ($self, $object, $destination) = @_;

    my $want_return = @_ == 2;
    my ($rc, $out, $err);
    $destination = \$out if $want_return;

    my $file;
    if (ref $destination)	# scalar ref, code ref, ....
    {
	(undef, $file) = tempfile();
    }
    else			# $destination is a filename
    {
	# avoid a double copy by writing directly to $destination
	# NOTE: CCM executes foo_cli_view_cmd chdir'ed somewhere,
	# convert $destination to an absolute pathname
	$file = File::Spec->rel2abs($destination);
    }

    # NOTE: cli_view_cmd must be specific to $object's cvtype,
    # otherwise it won't override the view_cmd attached to the attype.
    my $view_cmd = $object->cvtype . "_cli_view_cmd";

    ($rc, $out, $err) = $self->_ccm_with_option(
	$view_cmd => $^O eq 'MSWin32' ?
	    qq[cmd /c copy /b /y %filename "$file"] :  	#/
	    qq[$Config{cp} %filename '$file'],
	view => $object);
    unless ($rc == 0)
    {
	unlink $file if ref $destination;
	return $self->set_error($err || $out);
    }
    return 1 unless ref $destination;

    require IPC::Run3;
    my $type = IPC::Run3::_type($destination);
    if ($type eq "FH")
    {
	require File::Copy;
	File::Copy::copy($file, $destination);
    }
    else
    {
	open my $fh, "<$file"
	    or return $self->set_error("can't open temp file `$file': $!");
	binmode $fh;
	IPC::Run3::_read_child_output_fh("temp file", $type, $destination, $fh, {});
	close $fh;
    }
    unlink $file;
    return $want_return ? $out : 1;
}

# internal method
sub _attype_is_binary
{
    my ($self, $name) = @_;

    my $is_binary = $self->{attype_is_binary}->{$name};
    unless (defined $is_binary)
    {
	my ($result) = @{ $self->query_arrayref(
	    { cvtype => "attype", name => $name }, qw(binary)) };
	return $self->set_error("attype `$name' doesn't exist") unless $result;
	$self->{attype_is_binary}->{$name} = $is_binary = 
	    defined $result->[0] && $result->[0] eq "TRUE" ? 1 : 0;
    }
    return $is_binary;
}


sub types
{
        my $self = shift;
	my ($rc, $out, $err) = $self->_ccm(qw/show -types/);
	return $self->set_error($err || $out) unless $rc == 0;
	return split(/\n/, $out);
}


sub migrate_auto_rules
{
        my $self = shift;
	my ($rc, $out, $err) = $self->_ccm(qw/show -migrate_auto_rules/);
	return $self->set_error($err || $out) unless $rc == 0;
	return map { [ split(/ /, $_) ] } split(/\n/, $out);
}


sub ls
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(ls => @_);
    return $self->set_error($err || $out) unless $rc == 0;

    # filter out messages that a file has been implicitly synced 
    return [ grep { !/^\tUpdating database/ } split(/\n/, $out) ];
}


sub ls_arrayref
{
    my $self = shift;
    _usage(@_, 1, undef, '$file_spec, @keywords');

    my $file_spec = shift;
    return $self->_ls($file_spec, \@_, ROW_ARRAY);
}


sub ls_hashref
{
    my $self = shift;
    _usage(@_, 1, undef, '$file_spec, @keywords');

    my $file_spec = shift;
    return $self->_ls($file_spec, \@_, ROW_HASH);
}


sub ls_object
{
    my $self = shift;
    _usage(@_, 0, 1, '[ $file_spec ]');

    my $file_spec = shift;
    $file_spec = '.' unless defined $file_spec;

    return $self->_ls($file_spec, [], ROW_OBJECT);
}


sub _ls
{
    my ($self, $file_spec, $keywords, $row_type) = @_;

    my $want = _want($row_type == ROW_OBJECT, $keywords);
    
    my $format = $RS . join($FS, values %$want) . $FS;

    my ($rc, $out, $err) = $self->_ccm(qw/ls -format/, $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    # filter out messages that a file has been implicitly synced 
    $out =~ s/^\tUpdating database.*?(?:\n|\z)//m;

    my @result;
    foreach (split(/\Q$RS\E/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\Q$FS\E/, $_, -1);	# don't strip empty trailing fields
	my $row = $self->_parse_query_result($want, \@cols, $row_type == ROW_OBJECT);
	push @result, $row;
    }

    if ($row_type == ROW_ARRAY)
    {
	$_ = [ @$_{@$keywords} ] foreach @result;
    }
    return \@result;
}

    
sub set
{
    my $self = shift;
    _usage(@_, 0, 2, '[$option [, $value]]');

    my ($option, $value) = @_;
    if (@_ == 0)
    {
	my ($rc, $out, $err) = $self->_ccm(qw/set/);
	return $self->set_error($err || $out) unless $rc == 0;

	my %options;
	while ($out =~ /^(\S+) = (.*)$/gm)
	{
	    $options{$1} = $2 eq "(unset)" ? undef : $2;
	}
	return \%options;
    }

    my ($rc, $out, $err);
    my $old_value;

    # no need to get old value if we are called in void context
    if (defined wantarray)
    {
	my ($rc, $out, $err) = $self->_set($option);
	return $self->set_error($err || $out) unless $rc == 0;
	$old_value = $out;
    }

    if (@_ == 2)
    {
	my ($rc, $out, $err) = $self->_set($option, $value);
	return $self->set_error($err || $out) unless $rc == 0;
    }
    
    return $old_value;
}

sub _set
{
    my ($self, $option, $new_value) = @_;

    if (@_ == 2)
    {
	my ($rc, $out, $err) = $self->_ccm(set => $option);
	$out = undef if $rc == 0 &&  $out eq "(unset)";
	return ($rc, $out, $err);
    }

    if (@_ == 3)
    {
	my ($rc, $out, $err) = defined $new_value ?
	    $self->_ccm(set => $option, $new_value) :
	    $self->_ccm(unset => $option);
	return ($rc, $out, $err);
    }
    
    return _error("wrong number of arguments");
}


# helper: save value of $option, set it to $new_value, 
#  call _ccm(@args), restore $option; returns ($rc, $out, $err)
#  (usually the return value from _ccm(@args) except there were errors
#  in setting the option)
sub _ccm_with_option
{
    my ($self, $option, $new_value, @args) = @_;

    my ($rc, $out, $err);

    WITH_OPTION:
    {
	($rc, $out, $err) = $self->_set($option);
	last WITH_OPTION unless $rc == 0;
	my $old_value = $out;

	($rc, $out, $err) = $self->_set($option, $new_value);
	last WITH_OPTION unless $rc == 0;

	my @result = $self->_ccm(@args);

	($rc, $out, $err) = $self->_set($option, $old_value);
	last WITH_OPTION unless $rc == 0;

	($rc, $out, $err) = @result;
    }

    return ($rc, $out, $err);
}

# helper: write text to temporary file and return its name
# BEWARE: may re-use the same temporary file (deleted on script exit)
sub _text_to_tempfile
{
    my ($self, $text) = @_;

    my $fh;
    if ($self->{_tempfile})
    {
	open $fh, "> $self->{_tempfile}"
	    or return $self->set_error(qq[can't open temp file "$self->{_tempfile}": $!]); #'
    }
    else
    {
	($fh, $self->{_tempfile}) = tempfile(UNLINK => 1)
	    or return $self->set_error(qq[can't create temp file: $!]); #'
    }
    print $fh $text;
    close $fh;

    return $self->{_tempfile};
}

# helper: implements ye olde text_editor trick for ccm commands
# that would interactively open an editor in order to let the user modify
# some (text) value; ccm_with_text_editor writes $text_value 
# to a temporary file, then calls ccm_with_option with
# text_editor="cp temporary_file %filename" and returns its results
# calls $self->_ccm(@args).
sub ccm_with_text_editor
{
    my ($self, $text, @args) = @_;

    my $tempfile = $self->_text_to_tempfile($text) or return;

    # NOTE: 
    # (1) $Config{cp} is "copy" on Win32, but CMSynergy doesn't invoke
    #     the command processor on Windows when executing user
    #     callbacks like "text_editor"; thus "shell" builtins like "copy"
    #     (and redirection) won't work in user callbacks; hence 
    #     prefix it with "cmd /c" (use "/b" to get a binary copy
    #     and "/y" to overwite files without prompting)
    # (2) $tempfile is safe wrt cygwin, because $Config{cp} is
    #     a cygwin program ("/usr/bin/cp") on cygwin.
    my ($rc, $out, $err) = $self->_ccm_with_option(
	text_editor => $^O eq 'MSWin32' ?
	    qq[cmd /c copy /b /y "$tempfile" %filename] :		#/
	    qq[$Config{cp} '$tempfile' %filename],
	@args);
    return $self->set_error($err || $out) unless $rc == 0;
    return wantarray ? ($rc, $out, $err) : 1;
}


# [DEPRECATE < 6.3]
sub get_releases
{
    my ($self) = @_;

    my ($rc, $out, $err) = $self->_ccm(qw/releases -show/);
    return $self->set_error($err || $out) unless $rc == 0;

    my %releases;
    foreach (split(/\n/, $out))
    {
	next if /^\s*$/;
	my ($release, @names) = split(/\s*[:,]\s*/);
	$releases{$release} = [ @names ];
    }
    return \%releases;
}


# [DEPRECATE < 6.3]
sub set_releases
{
    my $self = shift;
    _usage(@_, 1, 1, '\\%releases');

    my $releases = shift;
    my $text = "";
    {
	local $" = ", ";
	while (my ($release, $names) = each %$releases) 
	{
	    $text .= "$release: @$names\n";
	}
    }

    my ($rc, $out, $err) =
	$self->ccm_with_text_editor($text, qw/releases -edit/);
    return $rc == 0 || $self->set_error($err || $out);
}


__PACKAGE__->_memoize_method(dcm_delimiter => sub
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(qw/dcm -show -delimiter/);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out;
});


__PACKAGE__->_memoize_method(dcm_database_id => sub
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(qw/dcm -show -database_id/);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out;
});


sub dcm_enabled		{ shift->dcm_database_id ne ""; }


__PACKAGE__->_memoize_method(default_project_instance => sub
{
    my $self = shift;
    return $self->version >= 6.3 && $self->dcm_enabled ?
	$self->dcm_database_id . $self->dcm_delimiter . '1' : '1';
});


sub _projspec2objectname
{
    my ($self, $project) = @_;
    $project .= ':project:' . $self->default_project_instance
	unless $project =~ /:project:/;
    return $project;
}


# generic wrapper for undefined method "foo":
# 	$ccm->foo(@args)
# gets turned into
# 	$ccm->ccm("foo", @args)
# in fact, we create a method `foo' on the fly with this definition
sub AUTOLOAD
{
    my ($this) = @_;

    our $AUTOLOAD;

    # NOTE: the fully qualified name of the method has been placed in $AUTOLOAD
    my ($class, $method) = $AUTOLOAD =~ /^(.*)::([^:]*)$/;
    return if $method eq 'DESTROY'; 

    # we don't allow autoload of class methods
    croak(qq[Can't locate class method "$method" via class "$class"]) #'
	unless ref $this;
    $Debug && $this->trace_msg(qq[autoloading method "$method"\n], 5);

    # create the new method on the fly
    no strict 'refs';
    *{$method} = sub 
    {
	my $self = shift;

	my ($rc, $out, $err) = $self->_ccm($method, @_);

	return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
	return $self->set_error($err || $out, undef, 0, $rc, $out, $err);
    };

    # call it w/o pushing a new stack frame (with same parameters)
    goto &$method;
}


# test whether session is still alive (without causing an exception)
sub ping
{
    my ($rc) = shift->_ccm(qw/delimiter/);
    return $rc == 0;
}


# $ccm->object(objectname) => VCS::CMSynergy::Object
# $ccm->object(name, version, cvtype, instance) => VCS::CMSynergy::Object
sub object
{
    my $self = shift;

    croak(__PACKAGE__."::object: invalid number of arguments" .
          "\n  usage: \$ccm->object(\$name, \$version, \$cvtype, \$instance)" .
          "\n  or     \$ccm->object(\$objectname)")
	unless @_ == 1 || @_ == 4;
    
    return VCS::CMSynergy::Object->new($self, @_) if @_ == 4;

    my $objectname = shift;
    return VCS::CMSynergy::Object->new($self, $1, $2, $3, $4)
	if $objectname =~ /$self->{objectname_rx}/;

    return $self->set_error("invalid objectname `$objectname'");
}

# convenience methods to get the base model object etc
# NOTE: base_model should actually be determined from attribute "active_model"
# of "default-1:admin:AC" (the value is an old-style fullname,
# but I've never seen anything else than "base/model/base/1").
sub base_model	{ $_[0]->object(qw(base 1 model base)); }
sub base_admin	{ $_[0]->object(qw(base 1 admin base)); }
sub dcm_admin	{ $_[0]->object(qw(dcm 1 admin dcm)); }
sub cs_admin	{ $_[0]->object(qw(cs 1 admin 1)); }
sub cvtype	{ $_[0]->object($_[1], qw(1 cvtype base)); }
sub attype	{ $_[0]->object($_[1], qw(1 attype base)); }

# FIXME: instead of implementing the inverse function to the
# ACcent method "displayname" of folder/task/problem objects, one could use
#    $self>query_object("query_function('$displayname')");
# but query functions like folder() didn't appear before CCM 6.x;
sub _displayname2object
{
    my ($self, $name, $cvtype, $format, $subsys) = @_;

    # displayname is either <number> (for a local object)
    # or <dbid><dcm_delimiter><number> (for a foreign object)
    if ($self->dcm_enabled)
    {
	$self->{dcm_prefix_rx} ||= do { my $rx = quotemeta($self->dcm_delimiter); qr/$rx/; };
	my @parts = split($self->{dcm_prefix_rx}, $name);
	if (@parts == 2)	{ ($subsys, $name) = @parts; }
	else			{ $subsys = $self->dcm_database_id; }
    }

    return $self->object(sprintf($format, $name), "1", $cvtype, $subsys);
}

# get folder/task/problem/... object from displayname (without querying Synergy)
sub folder_object				# folder('id')
{ 
    $_[0]->_displayname2object($_[1], qw/folder %s probtrac/); 
}
sub task_object					# task('id')
{ 
    $_[0]->_displayname2object($_[1], qw/task task%s probtrac/); 
}	
sub cr_object					# cr('id')
{ 
    $_[0]->_displayname2object($_[1], qw/problem problem%s probtrac/); 
}	
sub baseline_object				# baseline('id')
{ 
    $_[0]->_displayname2object($_[1], qw/baseline %s 1/); 
}	
sub project_object
{
    $_[0]->object($_[0]->_projspec2objectname($_[1]));
}


# $ccm->object_other_version(object, version) => VCS::CMSynergy::Object
#	new Object with same name/cvtype/instance as OBJECT, but version VERSION
sub object_other_version
{
    my $self = shift;
    _usage(@_, 2, 2, '$object, $other_version');

    my ($object, $other_version) = @_;
    return $self->object($object->name, $other_version, $object->cvtype, $object->instance);
}


# $ccm->object_from_cvid(cvid) => VCS::CMSynergy::Object
sub object_from_cvid
{
    my $self = shift;
    _usage(@_, 1, undef, '$cvid, @keywords');

    my $cvid = shift;
    return $self->_property("\@=$cvid", \@_, 1);
    # NOTE: if the cvid doesn't exist, "ccm property ..." has exit code 0, but 
    # "Warning: Object version representing type does not exist." on stderr
}


# $ccm->object_from_proj_ref($path, $proj_spec) => VCS::CMSynergy::Object
sub object_from_proj_ref
{
    my $self = shift;
    _usage(@_, 2, undef, '{ $path | \\@path_components }, $proj_spec, @keywords');

    my ($path, $proj_spec) = splice @_, 0, 2;
    $path = join(VCS::CMSynergy::Client::_pathsep, @$path) if ref $path; 
    return $self->_property("$path\@$proj_spec", \@_, 1);
    # NOTE/FIXME: no error if path isn't bound? possible errors:
    #   Specified project not found in database: '$self'
    #   Object version could not be identified from reference form: '$path'
}


1;

