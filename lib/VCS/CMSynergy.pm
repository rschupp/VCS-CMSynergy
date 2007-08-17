package VCS::CMSynergy;

our $VERSION = do { (my $v = q%version: 1.27.3 %) =~ s/.*://; sprintf("%d.%02d", split(/\./, $v), 0) };

use 5.006_000;				# i.e. v5.6.0
use strict;

use VCS::CMSynergy::Client qw(
    is_win32 $Debug $Error $Ccm_command %new_opts 
    _exitstatus _error _usage);
our @ISA = qw(VCS::CMSynergy::Client);


use Carp;
use Config;
use Cwd;
use File::Spec;
use File::Temp qw(tempfile);		# in Perl core v5.6.1 and later
use IPC::Run3;


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

    my %new_args;
    foreach (keys %args)
    {
	$new_args{$_} = delete $args{$_} if exists $new_opts{$_};
    }
    return $class->_start(VCS::CMSynergy::Client->new(%new_args), %args);
}


sub _start
{
    my ($class, $client, %args) = @_;
    croak("_start: $client is not a VCS::CMSynergy::Client")
	unless UNIVERSAL::isa($client, 'VCS::CMSynergy::Client');

    # make a deep clone of $client 
    my $self = { %$client };
    $self->{env} = { %{ $client->{env} } } if $client->{env};
    bless $self, $class;

    my @start = qw/start -m -q -nogui/;
    while (my ($arg, $value) = each %args)
    {
	return $self->set_error("unrecognized attribute `$arg'") 
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
	if ($self->{coprocess} = $self->_spawn_coprocess)
	{
	    $self->{cwd} = getcwd();	# remembers coprocess' working directory
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


sub database	
{ 
    my $self = shift;

    # determine database path (in canonical format) from `ccm ps'
    my $ccm_addr = $self->ccm_addr;
    my $ps = $self->ps(rfc_address => $ccm_addr);
    return $self->set_error("can't find session `$ccm_addr' in `ccm ps'") 
	unless $ps && @$ps > 0;
    return $ps->[0]->{database};
}
__PACKAGE__->_memoize_method('database');


use constant ROW_ARRAY	=> 0;
use constant ROW_HASH	=> 1;
use constant ROW_OBJECT	=> 2;

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
    my ($self, $query, @keywords) = @_;
    _usage(3, undef, '$query, $keyword...', \@_);

    return $self->_query($query, \@keywords, ROW_ARRAY);
}


sub query_hashref
{
    my ($self, $query, @keywords) = @_;
    _usage(3, undef, '$query, $keyword...', \@_);

    return $self->_query($query, \@keywords, ROW_HASH);
}


sub query_object
{
    my ($self, $query) = @_;
    _usage(2, 2, '$query', \@_);

    return $self->_query($query, [ 'object' ], ROW_OBJECT);
}


sub query_object_with_attributes
{
    my ($self, $query, @attributes) = @_;
    _usage(2, undef, '$query, $attribute...', \@_);
    # FIXME illegal @attributes: object objectname task_object find_use
    return $self->query_object($query) unless use_cached_attributes();

    return $self->_query($query, [ 'object', @attributes ], ROW_OBJECT);
}


sub query_count
{
    my ($self, $query) = @_;
    _usage(2, 2, '$query', \@_);

    my ($rc, $out, $err) = $self->_ccm(
	qw/query -u -ns -nf -format X/, $self->_expand_query($query));

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return 0 if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $out =~ tr/X/X/ if $rc == 0;			# count 'em X's
    return $self->set_error($err || $out);
}


# helper method: query with correct handling of multi-line attributes
sub _query
{
    my ($self, $query, $keywords, $row_type) = @_;

    $query = $self->_expand_query($query);

    my $want = _want($keywords);
    my $want_finduse = delete $want->{finduse};

    # NOTE: We use \cA and \cD as record/field separators.
    # Change Synergy uses \x1C-\x1E in attribute
    # "transition_log" of "problem" objects, so these are out.
    # Also people have been known to enter strange characters
    # like \cG even when using a GUI exclusively.
    my $format = "\cA" . join("\cD", values %$want) . "\cD";

    my ($rc, $out, $err) = $want_finduse ?
	$self->ccm_with_option(
	    Object_format => $format, 
	    qw/finduse -query/ => $query) :
	$self->_ccm( 
	    qw/query -u -ns -nf -format/ => $format, $query);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\cA/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\cD/, $_, -1);	# don't strip empty trailing fields
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
	    # other than '1' (either for DCM reasosns, or because someone
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
		    $project .= ':project:' . $self->default_project_instance
			unless $project =~ /:project:/;
		    $finduse{$project} = $path;
		}
	    }
	}

	my $row = $self->_parse_query_result($want, \@cols, $row_type);
	$row->{finduse} = \%finduse if $want_finduse;

	push @result, $row_type == ROW_ARRAY ? [ @$row{@$keywords} ] : $row;
    }

    return \@result;
}

sub _parse_query_result
{
    my ($self, $want, $cols, $row_type) = @_;

    my %row;
    
    # strip trailing newline (for consistency with get_attribute()),
    # translate "<void>" to undef and fill into correct slots;
    # @cols are in the same order as keys %$want
    @row{keys %$want} = map { s/\n\z//; /^<void>$/ ? undef : $_ } @$cols;
    
    # handle special keywords

    # Sigh. "ccm query -f %objectname" returns old-style fullnames
    # (i.e. "instance/cvtype/name/version") for certain types of 
    # objects, e.g. "cvtype" and "attype". But CM Synergy
    # doesn't accept these where a "file_spec" is expected 
    # (at least on Unix, because they contain slashes). 
    # Hence rewrite these fullnames to objectnames.
    for (qw(objectname object))
    {
	if ($want->{$_})
	{
	    # rewrite fullname if necessary
	    $row{$_} =~ s{^(.*?)/(.*?)/(.*?)/(.*?)$}
			 {$3$self->{delimiter}$4:$2:$1};
	}
    }

    if ($want->{task_objects})
    {
	# split comma-separated list of task numbers and objectify them
	$row{task_objects} = 
	    [ map { $self->task_object($_) } split(/,/, $row{task_objects}) ];
    }

    if ($want->{object})
    {
	# objectify column
	$row{object} = $self->object($row{object});
    }

    if ($row_type == ROW_OBJECT)
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

# helper: expand shortcut queries
sub _query_shortcut
{
    my ($self, $hashref) = @_;

    $Debug >= 5 && $self->trace_msg(
	"shortcut query { ".join(", ", map { "$_ => $hashref->{$_}" } keys %$hashref)." }\n", 5);

    my @clauses;
    while (my ($key, $value) = each %$hashref)
    {
	if (ref $value eq '')
	{
	    for ($key)
	    {
		/^task$/ && do 		# same as "ccm query -task ..."
		{
		    push @clauses, "is_associated_cv_of(cvtype='task' and task_number='$value')";
		    next;
		};
		/^match$/ && do
		{
		    push @clauses, "name match '$value'";
		    next;
		};
		# FIXME: rumor (D. Honey) has it that using
		#   "has_cvtype('base/cvtype/FOO/1')"
	        # is faster than plain
		#   "type='FOO'"
		# so we should treat $key =~ /^(cv?)type$/ specially, too
		push @clauses, "$key="._quote_value($value);
	    }
	}
	elsif (ref $value eq 'ARRAY')
	{
	    my $args = join(",", map { _quote_value($_) } @$value);
	    push @clauses, "$key($args)";
	}
	elsif (ref $value eq 'HASH')
	{
	    my $nested = $self->_query_shortcut($value);
	    push @clauses, "$key($nested)";
	}
	else
	{
	    (my $method = (caller(1))[3]) =~ s/^.*:://;
	    croak("$method: dunno how to handle $key => ".(ref $value)." in shortcut query");
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

# helper (not a method): build "want" array from keyword list (common case)
sub _want
{
    my ($keywords) = @_;
    my %want = map { $_ => "%$_" } @$keywords;
    $want{object} = "%objectname" if $want{object};
    $want{task_objects} = "%task" if $want{task_objects};
    return \%want;
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
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, \@keywords, ROW_ARRAY);
}


sub history_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, \@keywords, ROW_HASH);
}

# helper: history with correct handling of multi-line attributes
sub _history
{
    my ($self, $file_spec, $keywords, $row_type) = @_;

    my $want = _want($keywords);
    my $want_predecessors = delete $want->{predecessors};
    my $want_successors = delete $want->{successors};

    my $format = "\cA" . join("\cD", values %$want) . "\cD";

    my ($rc, $out, $err) = $self->_ccm(qw/history -f/, $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\cA/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\cD/, $_, -1);	# don't strip empty trailing fields
	
	# history information is the last "column"
	my $history = pop @cols;

	my $row = $self->_parse_query_result($want, \@cols, ROW_HASH);

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

	push @result, $row_type == ROW_ARRAY ? [ @$row{@$keywords} ] : $row;
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
		$project .= ':project:' . $self->default_project_instance
		    unless $project =~ /:project:/;
		$uses->{$project} = $path;
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

    my %orig_args = %args;
    foreach (qw/from_attributes to_attributes/)
    {
	croak(__PACKAGE__."::relations_hashref: optional $_ must be an array ref")
	    if exists $args{$_} && !UNIVERSAL::isa($args{$_}, 'ARRAY');

	# default keyword "objectname"
	$args{$_} ||= [ qw/objectname/ ];
    }

    my $result = $self->_relations(\%args, ROW_HASH);
    return unless $result;

    # if we defaulted "objectname" above, replace the corresponding
    # hash containing the sole key "objectname" with its value
    foreach my $what (qw/from to/)
    {
	unless (exists $orig_args{"${what}_attributes"})
	{
	    $_->{$what} = $_->{$what}->{objectname} foreach @$result;
	}
    }
    return $result;
}


sub relations_object
{
    my ($self, %args) = @_;

    foreach (qw/from_attributes to_attributes/)
    {
	croak(__PACKAGE__."::relations_object: optional $_ must be an array ref")
	    if exists $args{$_} && !UNIVERSAL::isa($args{$_}, 'ARRAY');

	# make a copy and add keyword "object"
	$args{$_} = [ @{ $args{$_} || [] }, "object" ];	
    }

    return $self->_relations(\%args, ROW_OBJECT);
}


# helper method: synthesize command and parse result of "ccm relate -show ..."
sub _relations
{
    my ($self, $args, $row_type) = @_;
    # NOTE: $args->{from_attributes}/$args->{to_attributes} must not be undef

    my $want_from = _want($args->{from_attributes});
    my $ncol_from = keys %$want_from;
    my $want_to = _want($args->{to_attributes});
    my $ncol_to = keys %$want_to;

    # NOTE: If the "from" part (the part before "::") of the format
    # or the "to" part are empty, Synergy may default it from
    # the other part. Hence both "from" and "to" part below are never
    # empty, even if $want_from or $want_to are empty.
    my $format = 
	"\cA" . 			# record delimiter
	join("\cD", 			# column separator
	    values %$want_from,		# "from" part
	    "::", 			# will be replaced by name of relation
	    values %$want_to) .		# "to" part
	"\cD";				# will be followed by create_time

    my ($rc, $out, $err) = $self->_ccm(
	qw/relate -show -format/ => $format, 
	    map { defined $args->{$_} ? ( "-$_" => $args->{$_}) : () } 
		qw/from to name/);

    # NOTE: if there are no hits, `ccm relate -show' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;

    my (@result, $from, $to);
    foreach (split(/\cA/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\cD/, $_, -1);	# don't strip empty trailing fields

	# first $ncol_from columns are the "from" part;
	# avoid to parse "from" part more than once # if "from => ..." was specified
	my @cols_from = splice @cols, 0, $ncol_from;
	$from = $self->_parse_query_result($want_from, \@cols_from, $row_type)
	    unless $args->{from} && $from;

	# next column is the name of the relation; trim whitespace
	(my $name = shift @cols) =~ s/^\s+|\s+$//g;	

	# next $ncol_to columns are the "to" part;
	# avoid to parse "to" part more than once if "to => ..." was specified
	my @cols_to = splice @cols, 0, $ncol_to;
	$to = $self->_parse_query_result($want_to, \@cols_to, $row_type)
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


# tied array class that acts as a readonly front to a real array
{
    package Tie::ReadonlyArray;	

    use Carp;

    sub TIEARRAY	{ bless $_[1], $_[0]; }
    sub FETCH		{ $_[0]->[$_[1]]; }
    sub FETCHSIZE	{ scalar @{$_[0]}; }
    sub AUTOLOAD	{ croak "attempt to modify a readonly array"; }
}


# put some items into the VCS::CMSynergy::Traversal namespace
{
    package VCS::CMSynergy::Traversal;

    our (@_dirs, @_projects);			# private
    our $_pathsep = $^O eq "MSWin32" ? "\\" : "/" ;

    our (@dirs, @projects, $prune);		# public
    tie @dirs,		"Tie::ReadonlyArray" => \@_dirs;
    tie @projects,	"Tie::ReadonlyArray" => \@_projects;

    sub path 
    { 
	my ($pathsep) = @_;
	$pathsep = $_pathsep unless defined $pathsep;

	return join($pathsep, map { $_->name } 
	                          @VCS::CMSynergy::Traversal::_dirs, $_); 
    }

    sub depth 
    { 
	return scalar @VCS::CMSynergy::Traversal::_dirs; 
    }
}


my %traverse_project_opts =
(
    wanted	=> "CODE",
    preprocess	=> "CODE",
    postprocess	=> "CODE",
    attributes	=> "ARRAY",
);

sub traverse_project
{
    my ($self, $wanted, $project, $dir) = @_;
    _usage(3, 4, '{ \\&wanted | \\%wanted }, $project [, $dir_object]', \@_);

    if (ref $wanted eq 'CODE')
    {
	$wanted = { wanted => $wanted };
    }
    elsif (ref $wanted eq 'HASH')
    {
	return $self->set_error("argument 1: option `wanted' is mandatory")
	    unless exists $wanted->{wanted};
	while (my ($key, $type) = each %traverse_project_opts)
	{
	    next unless exists $wanted->{$key};
	    return $self->set_error("argument 1: option `$key' must be a $type")
		unless UNIVERSAL::isa($wanted->{$key}, $type);
	}
    }
    else
    {
	return $self->set_error("argument 1 must be a CODE or HASH ref");
    }
    $wanted->{attributes} ||= [];

    if (ref $project)
    {
	return $self->set_error("argument 2 `$project' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($project, "VCS::CMSynergy::Object");
	return $self->set_error("argument 2 `$project' must have type `project'")
	    unless $project->is_project;
    }
    else
    {
	# treat $project as project_version string or an objectname
	$project .= ':project:' . $self->default_project_instance
	    unless $project =~ /:project:/;
	$project = $self->object($project);
    }

    if (defined $dir)
    {
	return $self->set_error("argument 3 `$dir' must be a VCS::CMSynergy::Object")
	    unless UNIVERSAL::isa($dir, "VCS::CMSynergy::Object");
	return $self->set_error("argument 3 `$dir' must have cvtype `dir'")
	    unless $dir->is_dir;

	# check that $dir is member of $project
	my $result = $self->query_object_with_attributes(
	    {
		name		=> $dir->name,
		cvtype		=> $dir->cvtype,
		instance	=> $dir->instance,
		version		=> $dir->version,
		is_member_of	=> [ $project ]
	    },
	    @{ $wanted->{attributes} });
	return $self->set_error("directory `$dir' doesn't exist or isn't a member of `$project'")
	    unless @$result;
	$dir = $result->[0];
    }
    else
    {
	my $result = $self->query_object_with_attributes(
	    { 
		name		=> $project->name,
		cvtype		=> $project->cvtype,
		instance	=> $project->instance,
		version		=> $project->version
	    }, 
	    @{ $wanted->{attributes} });
	return $self->set_error("project `$project' doesn't exist")
	    unless @$result;
	$dir = $result->[0];
    }

    local @VCS::CMSynergy::Traversal::_projects = ($project);
    local @VCS::CMSynergy::Traversal::_dirs = (); 
    $self->_traverse_project($wanted, $project, $dir);
}

# helper method: grunt work of traverse_project
sub _traverse_project
{
    my ($self, $wanted, $project, $parent) = @_;

    my $children = $self->query_object_with_attributes(
	{ is_child_of => [ $parent, $project ] }, 
	@{ $wanted->{attributes} }) or return;

    if ($wanted->{preprocess})
    {
        # make $_ the current dir/project during preprocess'ing 
	local $_ = $parent;
	{ $children = [ $wanted->{preprocess}->(@$children) ]; }
    }

    if (!$wanted->{bydepth}) 
    {
	local $_ = $parent;
	local $VCS::CMSynergy::Traversal::prune = 0;
	{ $wanted->{wanted}->(); }		# protect against wild "next"
	return 1 if $VCS::CMSynergy::Traversal::prune;
    }

    push @VCS::CMSynergy::Traversal::_dirs, $parent unless $parent->is_project;

    foreach (@$children)			# localizes $_
    {
	if ($_->is_project && $wanted->{subprojects})
	{
	    push @VCS::CMSynergy::Traversal::_projects, $_;
	    $self->_traverse_project($wanted, $_, $_) or return;
	    pop @VCS::CMSynergy::Traversal::_projects;
	    next;
	}
	if ($_->is_dir)
	{
	    $self->_traverse_project($wanted, $project, $_) or return;
	    next;
	}

	{ $wanted->{wanted}->(); }
    }

    pop @VCS::CMSynergy::Traversal::_dirs unless $parent->is_project;
    
    if ($wanted->{bydepth}) 
    {
        local $_ = $parent;
        local $VCS::CMSynergy::Traversal::prune = 0;
        { $wanted->{wanted}->(); }
	return 1 if $VCS::CMSynergy::Traversal::prune;
    }

    if ($wanted->{postprocess})
    {
        # make $_ the current dir/project during postprocess'ing 
	local $_ = $parent;
	{ $wanted->{postprocess}->(); }
    }

    return 1;
}


sub project_tree
{
    my ($self, $options, @projects) = @_;
    _usage(3, undef, '\\%options, $project...', \@_);

    $options = {} unless defined $options;
    return $self->set_error("argument 1 must be a HASH ref")
	unless ref $options eq "HASH";
    $options->{attributes} ||= [];
    my $mark_projects = delete $options->{mark_projects};
  
    # NOTE: $options->{attributes} and @projects will be checked 
    # by traverse_project() below

    my %tree;
    foreach my $tag (0 .. @projects-1)
    {
	$self->traverse_project(
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
	    }, $projects[$tag]) or return;
    }

    return \%tree;
}


sub get_attribute
{
    my ($self, $attr_name, $file_spec) = @_;
    _usage(3, 3, '$attr_name, $file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(qw/attribute -show/, $attr_name, $file_spec);
    return $out if $rc == 0;
    return if ($err || $out) =~ /Attribute .* does not exist on object/;
    return $self->set_error($err || $out);
}


sub set_attribute
{
    my ($self, $attr_name, $file_spec, $value) = @_;
    _usage(4, 4, '$attr_name, $file_spec, $value', \@_);

    my ($rc, $out, $err);

    if ($value eq "")
    {
	# FIXME: doesn't work on Windows (CCM seems to write/read
	# confirmation prompt and answer directly from the console
	# window, NOT from stdout/stdin
	return $self->set_error("setting a text attribute to an empty string is not supported on Windows")
	    if is_win32;

	# Setting a text attribute to an empty string is a real PITA:
	# - CM Synergy will launch text_editor, even if "-v ''" was specified
	# - if the temporary file containing the attribute's value is empty 
	#   after the editor exits, CM Synergy prompts with:
	#	Result of edit is an empty attribute.
	#	Confirm: (y/n) [n] 
	
	WITH_TEXT_EDITOR:
	{
	    ($rc, $out, $err) = $self->_set('text_editor');
	    last WITH_TEXT_EDITOR unless $rc == 0;
	    my $old_text_editor = $out;

	    ($rc, $out, $err) = $self->_set(
		text_editor => $^O eq 'MSWin32' ?
		    qq[cmd /c echo off > %filename ] :  	#/
		    qq[$Config{cp} /dev/null %filename]);
	    last WITH_TEXT_EDITOR unless $rc == 0;

	    $Error = $self->{error} = undef;
	    $Ccm_command = $self->{ccm_command} = "attribute -modify $attr_name $file_spec";

	    my $t0 = $Debug && [ Time::HiRes::gettimeofday() ];

	    local @ENV{keys %{ $self->{env} }} = values %{ $self->{env} };

	    run3([ $self->ccm_exe, qw/attribute -modify/, $attr_name, $file_spec ], 
		 \"y\n", \$out, \$err, 
		 { binmode_stdout => 1, binmode_stderr => 1 });
	    $rc = $?;
	    my @result = ($rc, $out, $err);

	    if ($Debug)
	    {
		my $elapsed = sprintf("%.2f", Time::HiRes::tv_interval($t0));
		if ($Debug >= 8)
		{
		    $self->trace_msg("<- ccm($self->{ccm_command})\n", 8);
		    $self->trace_msg("-> rc = $rc [$elapsed sec]\n", 8);
		    $self->trace_msg("-> err = \"$err\"\n", 8);
		}
		else
		{
		    my $success = $rc == 0 ? 1 : 0;
		    $self->trace_msg("ccm($self->{ccm_command}) = $success [$elapsed sec]\n");
		}
	    }

	    ($rc, $out, $err) = $self->_set(text_editor => $old_text_editor);
	    last WITH_OPTION unless $rc == 0;

	    ($rc, $out, $err) = @result;
	}
    }
    elsif (($self->{coprocess} && (length($value) > 1600 || $value =~ /["\r\n]/)) ||
        (is_win32 && (length($value) > 100 || $value =~ /[%<>&"\r\n]/)))
    {
	# Use ye olde text_editor trick if $value may cause problems
	# (depending on execution mode and platform) because its
	# too long or contains unquotable characters or...
	($rc, $out, $err) = $self->ccm_with_text_editor($value, 
	    qw/attribute -modify/, $attr_name, $file_spec);
    }
    else
    {
	($rc, $out, $err) = $self->_ccm(
	    qw/attribute -modify/, $attr_name, -value => $value, $file_spec);
    }

    return $self->set_error($err || $out) unless $rc == 0;
    return $value;
}


sub create_attribute
{
    my ($self, $name, $type, $value, @file_specs) = @_;
    _usage(5, undef, '$name, $type, $value, $file_spec...', \@_);
    croak(__PACKAGE__.'::create_attribute: argument $value must be defined')
	unless defined $value;

    # FIXME this should employ the same heuristic as set_attribute()
    # and use a separate ccm_with_text_editor(..., 'attribute -modify', ...)
    # for troublesome $value
    # FIXME need a variant "attribute -create -force ..."

    return scalar $self->ccm(qw/attribute -create/, $name, 
                             -value => $value, -type => $type, @file_specs);
}


sub delete_attribute
{
    my ($self, $name, @file_specs) = @_;
    _usage(3, undef, '$name, $file_spec...', \@_);

    return scalar $self->ccm(qw/attribute -delete/, $name, @file_specs);
}


sub copy_attribute
{
    my ($self, $name, @file_specs) = @_;
    _usage(4, undef, '{ $name | \\@names }, [ \\@flags, ] $from_file_spec, $to_file_spec...', \@_);

    $name = join(':', @$name) if UNIVERSAL::isa($name, 'ARRAY');

    my @flags = UNIVERSAL::isa($file_specs[0], 'ARRAY') ?
	map { "-$_" } @{ shift @file_specs } : ();
    
    return scalar $self->ccm(qw/attribute -copy/, $name, @flags, @file_specs);
}


sub list_attributes
{
    my ($self, $file_spec) = @_;
    _usage(2, 2, '$file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(qw/attribute -la/, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my %attrs = $out =~ /^(\S+) \s* \( (.*?) \)/gmx;
    return \%attrs;
}


sub property
{
    my ($self, $keyword, $file_spec) = @_;
    _usage(3, 3, '$keyword, $file_spec', \@_);

    # NOTE: CM adds a trailing blank on output
    my ($rc, $out, $err) = 
	$self->_ccm(qw/properties -nf -format/, "\cA%$keyword\cD", $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    local ($_) = $out =~ /\cA(.*)\cD/s or return undef;
    s/\n\z//;
    return /^<void>$/ ? undef : $_;
}


# FIXME $destination = \*FH or IO::Handle doesn't work 
# because of a bug in IPC::Run3 0.01
sub cat_object
{
    my ($self, $object, $destination) = @_;
    _usage(2, 3, '$object [, $destination]', \@_);
    return $self->set_error("argument 1 `$object' must be a VCS::CMSynergy::Object")
	unless UNIVERSAL::isa($object, "VCS::CMSynergy::Object");

    # [DEPRECATE < 6.3]
    if ($self->version < 6.3 && $self->_attype_is_binary($object->cvtype))
    {
	shift @_;			# so that omitting $destination works
	return $self->_cat_binary(@_);
    }

    my $want_return = @_ == 2;
    $destination = do { my $dummy; \$dummy; } if $want_return;

    $Error = $self->{error} = undef;
    $Ccm_command = $self->{ccm_command} = "cat $object";

    my $t0 = $Debug && [ Time::HiRes::gettimeofday() ];

    local @ENV{keys %{ $self->{env} }} = values %{ $self->{env} };
    local $?;				# don't screw up global $?
    my $err;

    run3([ $self->ccm_exe, 'cat', $object ], \undef, $destination, \$err, 
	 { binmode_stdout => 1, binmode_stderr => 1 });
    my $rc = $?;

    if ($Debug)
    {
	my $elapsed = sprintf("%.2f", Time::HiRes::tv_interval($t0));
	if ($Debug >= 8)
	{
	    $self->trace_msg("<- ccm($self->{ccm_command})\n", 8);
	    $self->trace_msg("-> rc = $rc [$elapsed sec]\n", 8);
	    $self->trace_msg("-> err = \"$err\"\n", 8);
	}
	else
	{
	    my $success = $rc == 0 ? 1 : 0;
	    $self->trace_msg("ccm($self->{ccm_command}) = $success [$elapsed sec]\n");
	}
    }
    return $self->set_error($err || "`ccm cat $object' failed") unless $rc == 0;
    return $want_return ? $$destination : 1;
}

# [DEPRECATE < 6.3]
sub _cat_binary
{
    my ($self, $object, $destination) = @_;
    my $want_return = @_ == 2;
    $destination = do { my $dummy; \$dummy; } if $want_return;

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

    my ($rc, $out, $err) = $self->ccm_with_option(
	$view_cmd => $^O eq 'MSWin32' ?
	    qq[cmd /c copy /b /y %filename "$file"] :  	#/
	    qq[$Config{cp} %filename '$file'],
	view => $object);
    unless ($rc == 0)
    {
	unlink $file if ref $destination;
	return $self->set_error($err || $out);
    }

    if (ref $destination)
    {
	local $?;
	run3([ $Config{cat}, $file ], \undef, $destination, \undef,
	     { binmode_stdout => 1 });
	unlink $file;
	return $want_return ? $$destination : 1;
    }
    else
    {
	return 1;
    }
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

    return [ split(/\n/, $out) ];
}


sub ls_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_ls($file_spec, \@keywords, ROW_ARRAY);
}


sub ls_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_ls($file_spec, \@keywords, ROW_HASH);
}


sub ls_object
{
    my ($self, $file_spec) = @_;
    _usage(1, 2, '[ $file_spec ]', \@_);
    $file_spec = '.' unless defined $file_spec;

    return $self->_ls($file_spec, [ qw/object/ ], ROW_OBJECT);
}


sub _ls
{
    my ($self, $file_spec, $keywords, $row_type) = @_;

    my $want = _want($keywords);
    
    my $format = "\cA" . join("\cD", values %$want) . "\cD";

    my ($rc, $out, $err) = $self->_ccm(qw/ls -format/, $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    foreach (split(/\cA/, $out))		# split into records 
    {
	next unless length($_);			# skip empty leading record

	my @cols = split(/\cD/, $_, -1);	# don't strip empty trailing fields
	my $row = $self->_parse_query_result($want, \@cols, $row_type);
	push @result, $row_type == ROW_ARRAY ? [ @$row{@$keywords} ] : $row;
    }
    return \@result;
}

    
sub set
{
    my ($self, $option, $value) = @_;
    _usage(1, 3, '[$option [, $value]]', \@_);

    if (@_ == 1)
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

    if (@_ == 3)
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
sub ccm_with_option
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

    return $self->set_error($err || $out) unless $rc == 0;
    return wantarray ? ($rc, $out, $err) : 1;
}

# helper: implements ye olde text_editor trick for ccm commands
# that would interactively open an editor in order to let the user modify
# some (text) value; ccm_with_text_editor writes $text_value 
# to a temporary file, then calls ccm_with_option with
# text_editor="cp temporary_file %filename" and returns its results
# calls $self->_ccm(@args).
sub ccm_with_text_editor
{
    my ($self, $text_value, @args) = @_;

    # try to re-use the temp file
    my $tempfile = $self->{ccm_with_text_editor_file};
    unless (defined $tempfile)
    {
	(undef, $tempfile) = tempfile();
	return $self->set_error("can't create temp file to set text value: $!")
	    unless defined $tempfile;

	push @{ $self->{files_to_unlink} }, $tempfile;
	$self->{ccm_with_text_editor_file} = $tempfile;
    }

    local *TEXT;
    open(TEXT, ">$tempfile")
	or return _error("can't open temp file `$tempfile' to set text value: $!");
    print TEXT $text_value;
    close(TEXT);

    # NOTE: 
    # (1) $Config{cp} is "copy" on Win32, but CMSynergy doesn't invoke
    #     the command processor on Windows when executing user
    #     callbacks like "text_editor"; thus "shell" builtins like "copy"
    #     (and redirection) won't work in user callbacks; hence 
    #     prefix it with "cmd /c" (use "/b" to get a binary copy
    #     and "/y" to overwite files without prompting)
    # (2) $tempfile is safe wrt cygwin, because $Config{cp} is
    #     a cygwin program ("/usr/bin/cp") on cygwin.
    return $self->ccm_with_option(
	text_editor => $^O eq 'MSWin32' ?
	    qq[cmd /c copy /b /y "$tempfile" %filename] :
	    qq[$Config{cp} '$tempfile' %filename],
	@args);
}


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


sub set_releases
{
    my ($self, $releases) = @_;
    _usage(2, 2, '\\%releases', \@_);

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


sub dcm_delimiter
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(qw/dcm -show -delimiter/);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out;
}
__PACKAGE__->_memoize_method('dcm_delimiter');


sub dcm_database_id
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm(qw/dcm -show -database_id/);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out;
}
__PACKAGE__->_memoize_method('dcm_database_id');


sub dcm_enabled		{ shift->dcm_database_id ne ""; }


sub default_project_instance
{
    my $self = shift;
    return $self->version >= 6.3 && $self->dcm_enabled ?
	$self->dcm_database_id . $self->dcm_delimiter . '1' : '1';
}
__PACKAGE__->_memoize_method('default_project_instance');


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
sub base_admin	{ $_[0]->object(qw(base 1 admin base)); }
sub base_model	{ $_[0]->object(qw(base 1 model base)); }
sub dcm_admin	{ $_[0]->object(qw(dcm 1 admin dcm)); }
sub cvtype	{ $_[0]->object($_[1], qw(1 cvtype base)); }
sub attype	{ $_[0]->object($_[1], qw(1 attype base)); }

# get foler object from displayname (without querying Synergy)
sub folder_object
{
    my ($self, $folder) = @_;

    # displayname is either <number> (for a local folder)
    # or <dbid><dcm_delimiter><number> (for a foreign folder)
    my ($instance, $num) = $folder =~ /^(?:(.*)\D)?(\d+)$/;
    $instance = $self->dcm_enabled ? $self->dcm_database_id : "probtrac"
	unless defined $instance;

    # FIXME: alternatively could use 
    #    $self>query_object({ folder => [ $folder ] }) };
    # but query function folder() appeared in CCM 6.x

    return $self->object($num, qw(1 folder), $instance);
}

# get task object from displayname (without querying Synergy)
sub task_object
{
    my ($self, $task) = @_;

    # displayname is either <number> (for a local task)
    # or <dbid><dcm_delimiter><number> (for a foreign task)
    my ($instance, $num) = $task =~ /^(?:(.*)\D)?(\d+)$/;
    $instance = $self->dcm_enabled ? $self->dcm_database_id : "probtrac"
	unless defined $instance;

    # FIXME: alternatively could use 
    #    $self>query_object({ task => [ $task ] }) };
    # but query function task() appeared in CCM 6.x

    return $self->object("task$num", qw(1 task), $instance);
}

# $ccm->object_other_version(object, version) => VCS::CMSynergy::Object
#	new Object with same name/cvtype/instance as OBJECT, but version VERSION
sub object_other_version
{
    my ($self, $object, $other_version) = @_;
    _usage(3, 3, '$object, $other_version', \@_);
    return $self->object($object->name, $other_version, $object->cvtype, $object->instance);
}

# $ccm->object_from_cvid(cvid) => VCS::CMSynergy::Object
sub object_from_cvid
{
    my ($self, $cvid) = @_;
    _usage(2, 2, '$cvid', \@_);

    # NOTES: 
    # - CM adds a trailing blank on output
    # - if the cvid doesn't exist, we get exit code = 0, but 
    #   "Warning: Object version representing type does not exist." on stderr
    my ($rc, $out, $err) = 
	$self->_ccm(qw/properties -nf -format/, "\cA%objectname\cD", "\@=$cvid");
    return $self->set_error($err || $out) unless $rc == 0;

    my ($name) = $out =~ /\cA(.*)\cD/s;
    return $self->set_error($err || $out) unless $name;

    return $self->object($name);
}

1;

