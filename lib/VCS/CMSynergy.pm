package VCS::CMSynergy;

our $VERSION = sprintf("%d.%02d", q%version: 1.14 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy - Perl interface to Telelogic CM Synergy (aka Continuus/CM)

=head1 SYNOPSIS

  use VCS::CMSynergy;

  $ccm = VCS::CMSynergy->new(%attr);

  ($rc, $out, $err) = $ccm->ccm($ccm_command, @ccm_args);
  ($rc, $out, $err) = $ccm->any_ccm_command(@ccm_args);

  $ary_ref = $ccm->query(@ccm_args);
  $ary_ref = $ccm->query_arrayref($query, @keywords);
  $ary_ref = $ccm->query_hashref($query, @keywords);
  $ary_ref = $ccm->query_object($query);

  $ary_ref = $ccm->finduse(@args);
  $path = $ccm->findpath($file_spec, $proj_vers);

  $ary_ref = $ccm->history(@ccm_args);
  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

  $ary_ref = $ccm->ls(@ccm_args);
  $ary_ref = $ccm->ls_object($file_spec);
  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->ls_hashref($file_spec, @keywords);

  $value = $ccm->get_attribute($attr_name, $file_spec);
  $ccm->set_attribute($attr_name, $file_spec, $value);
  $hash_ref = $ccm->list_attributes($file_spec);

  $delim = $ccm->delimiter;
  $database = $ccm->database;
  $ENV{CCM_ADDR} = $ccm->ccm_addr;
  @types = $ccm->types;

  $last_error = $ccm->error;
  $last_ccm_command = $ccm->ccm_command;

  ($ccm, $schema, $informix, @patches) = $h->version;
  @ary = $h->databases;
  $ary_ref = $h->ps;
  $ary_ref = $h->ps(\%attr);
  $ary_ref = $h->status;

This synopsis only lists the major methods.
Methods for administering users and their roles are
described in the L<VCS::CMSynergy::Users> documentation.

=cut

use 5.006_000;				# i.e. v5.6.0
use strict;
use Carp;

use Config;
use Cwd;
use File::Spec;
use File::Basename;
use IPC::Open3;
use POSIX qw(_exit);
use VCS::CMSynergy::Object;

# Unix only
use IO::Handle;
use IO::Select;
use IO::File;
use IO::Pipe;				# make ActiveState PerlApp happy

use File::Temp qw(tempfile);		# in Perl core v5.6.1 and later

BEGIN
{
    if ($^O eq 'cygwin')
    { 
	require Filesys::CygwinPaths; import Filesys::CygwinPaths qw(:all);
    }
}


our $ccm_prompt = qr/^ccm> /;		# NOTE the trailing blank
our $Is_MSWin32 = $^O eq 'MSWin32' || $^O eq 'cygwin';
our ($Debug, $Debugfh, $Error, $Ccm_command, $Out, $Err, $OneArgFoo);

{
    $Debug = $ENV{CMSYNERGY_TRACE} || 0;
    $Debugfh = IO::Handle->new_from_fd(\*STDERR, "w");
    if ($Debug)
    {
	if ($Debug =~ /^\d+$/) 			# CMSYNERGY_TRACE="digits"
	{ 
	    # level=digits, tracefile=stderr
	    __PACKAGE__->trace($Debug, undef); 	
	}
	elsif ($Debug =~ /^(\d+)=(.*)/) 	# CMSYNERGY_TRACE="digits=filename"
	{
	    # level=digits, tracefile=filename
	    __PACKAGE__->trace($1, $2); 
	}
	else					# CMSYNERGY_TRACE="filename"
	{
	    # level=2, tracefile=filename
	    __PACKAGE__->trace(2, $Debug); 	
	}
    }

    # Use Memoize.pm if it's available, otherwise define
    # sub VCS::CMSynergy::memoize as a no-op.
    eval { require Memoize; import Memoize; };
    *memoize = sub { 1; } if $@;
}


# FIXME document OneArgFoo stuff

sub new
{
    my $class = shift;
    my %args = @_;

    my $self = 
    {
	HandleError	=> undef,
	PrintError	=> 1,
	RaiseError	=> 0,
	KeepSession	=> undef,
	ccm_command	=> undef,
	error		=> undef,
	env		=> 
	{
	    CCM_HOME	=> $class->ccm_home
	},
    };	
    bless $self, $class;

    my @start = qw(start -m -q -nogui);
    while (my ($arg, $value) = each %args)
    {
	if (grep { $_ eq $arg } 
	      qw(HandleError PrintError RaiseError KeepSession UseCoprocess))
	{
	    $self->{$arg} = $value;
	    next;
	}
	if (grep { $_ eq $arg } qw(CCM_ADDR CCM_HOME))
	{
	    $self->{env}->{$arg} = $value;
	    next;
	}

	$arg eq "database"	&& do { push @start, "-d", $value; next; };
	$arg eq "home"		&& do { push @start, "-home", $value; next; };
	$arg eq "host"		&& do { push @start, "-h", $value; next; };
	$arg eq "ini_file"	&& do { $self->{ini_file} = $value; next; };
	$arg eq "password"	&& do { push @start, "-pw", $value; next; };
	$arg eq "remote_client" && do { push @start, "-rc" if $value; next; };
	$arg eq "role"		&& do { push @start, "-r", $value; next; };
	$arg eq "ui_database_dir" && do { push @start, "-u", $value; next; };
	$arg eq "user"		&& do 
	{
	    $self->{user} = $value;
	    push @start, "-n", $value; 
	    next; 
	};

	return $self->set_error("unrecognized attribute `$arg'") 
    }

    if (defined $self->ccm_addr)
    {
	$self->{KeepSession} = 1 unless defined $self->{KeepSession};
	$Debug && $self->trace_msg("will keep session `".$self->ccm_addr."'\n");

	if ($Is_MSWin32)
	{

	    # figure out user of session specified by CCM_ADDR
	    {
		$self->{user} = 
		    $self->ps(rfc_address => $self->ccm_addr)->[0]->{user};
	    }

	    # create a minimal ini file (see below for an explanation)
	    (my $inifh, $self->{ini_file}) = tempfile(SUFFIX => ".ini", UNLINK => 0);
	    $self->{ini_file} = fullwin32path($self->{ini_file}) if $^O eq 'cygwin';
	    			# because it's passed to ccm.exe
		
	    print $inifh "[UNIX information]\nUser = $self->{user}\n";
	    close($inifh);
	    push @{ $self->{files_to_unlink} }, $self->{ini_file};
	}
    }
    else
    {
	unless (defined $self->{ini_file})
	{
	    if ($Is_MSWin32)
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

	$self->{ccm_command} = $Ccm_command = join(" ", @start);
	my ($rc, $out, $err) = 
	    _ccmexec($self->{env}, _ccm_exe($self->ccm_home), @start);
	$Debug && $self->trace_msg(
	    "<- ccm($Ccm_command) = " .
	    ($Debug >= 8 ? "($rc, '$out', '$err')" : $rc==0) . "\n");
	return $self->set_error($err || $out) unless $rc == 0;

	$self->{env}->{CCM_ADDR} = $out;
	$Debug && $self->trace_msg("started session `$out'\n");
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
    $self->{env}->{CCM_INI_FILE} = $self->{ini_file} if $Is_MSWin32;

    if ($self->{UseCoprocess})
    {
	if ($self->{coprocess} = $self->_spawn_coprocess)
	{
	    $self->{cwd} = getcwd();	# remembers coprocess' working directory
	    $Debug && $self->trace_msg(
		"spawned coprocess (pid=".$self->{coprocess}->pid.")\n", 8);
	}
	else
	{
	    carp(__PACKAGE__ . " new: can't establish coprocess: $self->{error}\n" .
	         "-- ignoring UseCoprocess");
	}
    }

    # cache some info from database; this also doubles as a test for a valid session
    {
	my ($rc, $out, $err) = $self->_ccm(0, 'delimiter');
	return $self->set_error($err || $out) unless $rc == 0;
	$self->{delimiter} = $out;
    }

    $self->{objectname_rx} = qr/^(.*?)\Q$self->{delimiter}\E(.*?):(.*?):(.*?)$/;
    $self->{finduse_rx} = qr/(?m)^\t(.*?)\Q$self->{delimiter}\E.*?\@(.*?)$/;
    $self->{database} = undef;		# determine on demand

    if ($Debug >= 9)
    {
	require Data::Dumper;
	local $Data::Dumper::Useqq = 1;
	$self->trace_msg(Data::Dumper->Dump([$self], ["$self"]));
    }

    return $self;
}

sub _spawn_coprocess
{
    my $self = shift;

    eval { require Expect; import Expect 1.15; };
    $self->{error} = $@, return undef if $@;

    my $env = $self->{env};
    local @ENV{keys %$env} = values %$env if defined $env;

    my $exp = Expect->new
	or $self->{error} = "Expect->new failed", return undef;
    ($exp->log_stdout(0) && $exp->slave->set_raw && $exp->set_raw)
	or $self->{error} = $exp->exp_error, return undef;
    $exp->spawn(_ccm_exe($self->ccm_home))
	or $self->{error} = $exp->exp_error, return undef;
    
    # look for initial "ccm> " prompt
    $exp->expect(undef, -re => $ccm_prompt)
	or $self->{error} = $exp->exp_error, return undef;

    return $exp;
}

sub _kill_coprocess
{
    my $self = shift;
    $self->{coprocess}->print("exit\n");
    # FIXME: kill it just for paranoia (must save pid before line above!)
    $self->{coprocess} = undef;
}

sub DESTROY 
{
    my $self = shift;
    return unless $self->ccm_addr;	# session not yet established

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
    
    $self->_kill_coprocess if $self->{coprocess};

    unless ($self->{KeepSession})
    {
	$self->_ccm(0, 'stop');
	$Debug && $self->trace_msg("stopped session ".$self->ccm_addr."\n");

	# on Windows, wait a little until CM Synergy
	# really releases any files to unlink
	sleep(2) if $Is_MSWin32 && $self->{files_to_unlink};
    }

    unlink(@{ $self->{files_to_unlink} }) if $self->{files_to_unlink};

    %$self = ();			# paranoia setting
}

sub ccm					# class/instance method
{
    my $this = shift;

    my ($rc, $out, $err) = $this->_ccm($OneArgFoo && @_ == 1, @_);

    return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
    return $this->set_error($err || $out, undef, 0, $rc, $out, $err);
    # NOTE: most failing ccm commands issue there error messages on stdout!
}

# helper: just do it (TM), returns ($rc, $out, $err) (regardless of context)
sub _ccm					# class/instance method
{
    my ($this, $oneargfoo, $cmd, @args) = @_;
    $this->{error} = undef if ref $this;

    $Ccm_command = join(" ", $cmd, @args);
    $this->{ccm_command} = $Ccm_command if ref $this;

    my $rc;

    USE_COPROCESS:
    {
	last USE_COPROCESS unless ref $this && $this->{coprocess};

	# certain comands (e.g. "ps") are not accepted in "interactive" sessions
	last USE_COPROCESS if $cmd =~ /^\s*(ps|version)\b/;

	# arguments cannot contain newlines in "interactive" ccm sessions
	last USE_COPROCESS if grep { /\n/ } @args;

	# FIXME: calling getcwd for every _ccm may be expensive
	if ($this->{cwd} ne (my $cwd = getcwd()))
	{
	    # working directory has changed since coprocess was spawned:
	    # shut down coprocess and start a new one
	    # NOTE: don´t call _ccm here (infinite recursion)
	    $this->_kill_coprocess;
	    if ($this->{coprocess} = $this->_spawn_coprocess)
	    {
		$this->{cwd} = $cwd;	# remembers coprocess' working directory	
		$Debug && $this->trace_msg(
		    "spawned new coprocess because cwd changed (pid=".$this->{coprocess}->pid.")\n", 8);
	    }
	    else
	    {
		carp(__PACKAGE__ . " _ccm: can't re-establish coprocess (because cwd changed): $this->{error}\n" .
		     "-- ignoring UseCoprocess from now on");
		last USE_COPROCESS;
	}
    }

	my ($match, $set);

	# NOTE: "interactive" command arguments that contain blanks must 
	# be quoted with double quotes; AFAICT there is no way 
	# to quote embedded quotes!
	$this->{coprocess}->print(
	    join(" ", $cmd, $oneargfoo ? @args : map { "\"$_\"" } @args), "\n");

	($match, $Err, undef, $Out, undef) =
	    $this->{coprocess}->expect(undef, -re => $ccm_prompt);
	($rc, $Out, $Err) = _error("expect error: $Err"), last CLI 
	    unless $match;

	# on Windows, treat output as if read in "text" mode
	$Out =~ s/\015\012/\012/g if $Is_MSWin32;
	chomp($Out);

	$this->{coprocess}->print("set error\n");
	($match, $Err, undef, $set, undef) =
	    $this->{coprocess}->expect(undef, -re => $ccm_prompt);
	($rc, $Out, $Err) = _error("expect error: $Err"), last CLI 
	    unless $match;
	($rc, $Out, $Err) = _error("unrecognized result from `set error': $set"), last CLI
	    unless ($rc) = $set =~ /^(\d+)/;
	($rc, $Err) = (_exitstatus($rc), "");

	$Debug && $this->trace_msg(
	    "<- ccm($Ccm_command) = " .
	    ($Debug >= 8 ? "($rc, `$Out', `$Err')" : $rc==0) .  "\n");

	($this->{out}, $this->{err}) = ($Out, $Err) if ref $this;

	return ($rc, $Out, $Err);
    }

    # simple ccm sub process 
	($rc, $Out, $Err) = _ccmexec(
	ref $this ? 
	    $this->{env} : 
	    { CCM_HOME => $this->ccm_home },
	$oneargfoo ? 
	    join(" ", _ccm_exe($this->ccm_home), $cmd, @args) :
	    (_ccm_exe($this->ccm_home), $cmd, @args));
    $Debug && $this->trace_msg(
	"<- ccm($Ccm_command) = " .
        ($Debug >= 8 ? "($rc, `$Out', `$Err')" : $rc==0) .  "\n");

    ($this->{out}, $this->{err}) = ($Out, $Err) if ref $this;

    return ($rc, $Out, $Err);
}

# helper (not a method, hence memoizable): 
# return pathname to ccm executable
sub _ccm_exe
{
    my ($ccm_home) = @_;
    return File::Spec->catfile($ccm_home, qw(bin ccm));
}

memoize('_ccm_exe');

# helper (not a method):
# execute a program with environment set up appropriately
sub _ccmexec
{
    my ($env, $prog, @args) = @_;

    local (*NULL);			
    open(NULL, File::Spec->devnull) or die "can't open /dev/null: $!";
    # NOTE: NULL will be closed (in parent) by open3

    # NOTE: On operating systems with a broken "fflush(NULL)"
    # (e.g. Solaris), Perl does _not_ flush all open file handles
    # before a fork() (called by open3() below). Hence the user
    # might see "double output". The workaround below does not
    # completely solve the problem, but at least we can explicitly
    # flush all file handles we know about (STDOUT, STDERR and $Debugfh).
    unless ($Config{fflushNULL})
    {
	STDOUT->flush;
	STDERR->flush;
	$Debugfh->flush if defined $Debugfh;
    }

    my ($outfh, $errfh, $pid);
    if ($Is_MSWin32)
    {
	# NOTE: On Win32, `exec LIST´ (as called by open3) will mung
	# the LIST elements, e.g. an element with embedded blanks
	# will result in two or more arguments passed to the
	# exec'ed program, an embedded '>' will result in IO
	# redirection. This is a bug and may be fixed in Perl
	# versions later than 5.6.1 (cf. Changelog entries
	# #12563 and #12559). The workaround below
	# fixes blanks and IO redirectors, but doesn't help
	# for substrings like "%path%"
	# where the Windows shell does variable substitution even
	# when inside double quotes. FIXME
	# FIXME: This doesn't seem to be true any more, at least not for
	# ActivePerl build 631 running on Windows 2000.
	if ($^O eq 'MSWin32')
	{
	    foreach (@args)
	    {
		if (/[\s<|>"]/) { s/"/\\"/g; $_ = "\"$_\""; } 
	    }
	}
    }
    else
    {
	# NOTE: When open3 below is called with $outfh and $errfh undefined
	#       open3 will generate and assign filehandles, but
	#       it will assign the _same_ filehandle to $outfh and $errfh.
	$outfh = IO::Handle->new;
	$errfh = IO::Handle->new;
    }

    # Disable possible outer SIGCHLD handler.
    # FIXME: add NOTE why we do this (SIGCHLD probs eg RPC::PlServer)
    # FIXME: works on MSWin32 ?
    my $outer_sigchld_handler = $SIG{CHLD};
    {
	# Shut up "Use of uninitialized value in scalar assignment"
	# warnings (%SIG seems to be a special case here).
	no warnings qw(uninitialized);
	$SIG{CHLD} = undef;
    }
 
    eval 
    {
	# NOTE: 
	# (1) in case of failure, open3 die()s with $@ =~ /^open3:/
	# (2) if the exec fails in the child forked by open3,
	# 	  the child will die(); however, in this case we don't want
	# 	  the child to run END blocks or DESTROYs (esp. since this would
	# 	  `ccm stop' all sessions); the __DIE__ handler (inherited 
	# 	  by the child) suppresses this by calling POSIX::_exit
	# (3) since open3 may also die() in the parent, let
	#	  this die simply proceed in __DIE__ handler
	my $ppid = $$;
	local $SIG{__DIE__} = sub { 
	    return if $$ eq $ppid;	
	    print STDERR $_[0]; 
	    POSIX::_exit(255); 
	};
	local @ENV{keys %$env} = values %$env if defined $env;
	$pid = open3("<&NULL", $outfh, $errfh, $prog, @args);
    };
    return _error($@) if $@;

    my ($rc, $out, $err) = (undef, "", "");
    if ($Is_MSWin32)
    {
	# select() does not work on pipes in Win32,
	# hence IO::Select below is useless. In this case STDOUT and
	# STDERR of the child are connected to the same file handle
	# (since $outfh and $errfh were undefined when calling open3),
	# so we simply read $outfh until eof.
	local $/ = undef;
	$out = <$outfh>;
	close($outfh);
	$out = "" unless defined $out;
    }
    else
    {
	my $sel = IO::Select->new($outfh, $errfh);
	my $buf;
	while(my @ready = $sel->can_read())
	{
	    foreach my $fh (@ready)
	    {
		if ($fh->sysread($buf, 2048))
		{
		    no warnings qw(uninitialized);
		    $fh eq $outfh ? $out : $err .= $buf;
		}
		else
		{
		    # NOTE: remove handle BEFORE closing it:
		    # membership in $sel is actually based on fileno;
		    # if we close $fh before removal its fileno is gone;
		    # hence removal is a noop; this leaves an invalid 
		    # file descriptor in the select set which causes the whole
		    # can_read loop to exit prematurely
		    $sel->remove($fh);	
		    $fh->close;
		}
	    }
	}
    }

    return _error("waitpid returned unexpected value")
	if waitpid($pid, 0) != $pid;
    $rc = $?;
    if (my $sig = $rc & 127)
    {
	($out, $err) = ("", "Killed by signal $sig");
	$err .= " (core dumped)" if $rc & 128;
    }

    # on Windows, treat output as if read in "text" mode
    $out =~ s/\015\012/\012/g if $Is_MSWin32;

    chomp($out, $err);

    for ($outer_sigchld_handler)
    {
	# Re-establish outer SIGCHLD handler.
	{
	    no warnings qw(uninitialized);	# cf. above
	    $SIG{CHLD} = $_;
	}

	# SIG_DFL: nothing to do
	last if !defined $_ || $_ eq '' || $_ eq 'DEFAULT';

	# SIG_IGN: nothing to do (FIXME: must I reap all zombies?)
	last if $_ eq 'IGNORE';

	# "Real" (sub) handler (may be denoted by various means).
	# Call it explicitly, as child processes (other than $pid)
	# might have died since we diabled it (there will be no signal
	# emitted a posteriori for these zombies).
	
	# string (handler's name) or type glob: call it
	&$_("CHLD"), last unless ref $_; 
	
	# sub ref: call it
	&$_("CHLD"), last if ref $_ eq 'CODE';	
	
	carp(__PACKAGE__ . " __ccmexec: don't know how to call SIGCHLD handler of type " . ref $_);
    }

    return ($rc, $out, $err);
}

# helper: inverse function of POSIX::WEXITSTATUS()
sub _exitstatus { return $_[0] << 8; }

# helper: return a triple ($rc, $out, $err)
sub _error { return (_exitstatus(255), "", $_[0]) }

# helper: check usage
# check min ($minargs) and max ($maxargs) number of arguments 
# (numbers include $self); use $maxargs=undef for unlimited arguments;
# croak with message constructed from $usage
sub _usage
{
    my ($minargs, $maxargs, $usage, $argsref) = @_;
    return if $minargs <= @$argsref && (!defined $maxargs || @$argsref <= $maxargs);
    my $fullname = (caller(1))[3];
    (my $method = $fullname) =~ s/^.*:://;
    croak("$fullname: invalid number of arguments" .
          "\n  usage: \$ccm->${method}(${usage})");
}

sub ccm_addr	
{ 
    return shift->{env}->{CCM_ADDR}; 
}

sub ccm_command
{
    my $this = shift;
    return ref $this ? $this->{ccm_command} : $Ccm_command;
}

sub ccm_home					# class/instance method
{
    my $this = shift;
    return ref $this ? $this->{env}->{CCM_HOME} : $ENV{CCM_HOME};
}

sub out 					# class/instance method
{
    my $this = shift;
    my $out = ref $this ? $this->{out} : $Out;
    return wantarray ? split(/\n/, $out) : $out;
}

sub err 					# class/instance method
{
    my $this = shift;
    return ref $this ? $this->{err} : $Err;
}

sub database	
{ 
    my $self = shift;

    unless (defined $self->{database})
    {
	# determine database path (in canonical format) from `ccm ps´
	my $ccm_addr = $self->ccm_addr;
	my $ps = $self->ps(rfc_address => $ccm_addr);
	return $self->set_error("can't find session `$ccm_addr' in `ccm ps'") 
	    unless $ps && @$ps > 0;
	$self->{database} = $ps->[0]->{database};
    }
    
    return $self->{database}; 
}

sub delimiter	
{ 
    return shift->{delimiter}; 
}

sub error
{
    my $this = shift;
    return ref $this ? $this->{error} : $Error;
}

sub query
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, qw(query -u), @_);

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

    return $self->_query($query, 0, @keywords);
}

sub query_hashref
{
    my ($self, $query, @keywords) = @_;
    _usage(3, undef, '$query, $keyword...', \@_);

    return $self->_query($query, 1, @keywords);
}

# helper: query with correct handling of multi-line attributes and shorthand queries
sub _query
{
    my ($self, $query, $wanthash, @keywords) = @_;

    # expand shorthand queries
    # 
    # NOTE: CM Synergy seems to use the following quoting rules
    # for the right hand side of an attribute value clause in a query:
    # - string and text values must be quoted
    # - boolean values ("TRUE" or "FALSE") must not be quoted
    # - integer values must not be quoted, but must always have a leading sign
    # - time values must be written as "time('Fri Dec 12 1997')"
    if (ref $query eq 'HASH')
    {
	my @clauses;

	while (my ($key, $value) = each %$query)
	{
	    if (ref $value)				# i.e. [ type => value ]
	    {
		for ($value->[0])
		{
		    $value = $value->[1];
		    /^(string|text)$/ &&
			do { push @clauses, "$key = '$value'"; last; };
		    /^s?match$/ &&
			do { push @clauses, "$key $_ '$value'"; last; };
		    /^bool(ean)?$/ &&
			do { push @clauses, "$key = ". ($value ? "TRUE" : "FALSE"); last; };
		    /^int(eger)?$/ &&
			do { push @clauses, sprintf("%s = %+d", $key, $value); last; };
		    return $self->set_error("unknown qualifier `$_' in shorthand query");
		}
	    }
	    elsif ($value =~ /^(TRUE|FALSE)$/)		# treat as boolean
	    {
		push @clauses, "$key = $value";
	    }
	    else					# treat as string/text
	    {
		push @clauses, "$key = '$value'";
	    }
	}

	$query = join(" and ", @clauses);
    }

    my %wanted = map { ($_, "%$_") } @keywords;
    $wanted{object} = "%objectname"	if exists $wanted{object};
    $wanted{finduse} = "?"		if exists $wanted{finduse};

    # NOTE: We use \x01 and \x04 as record/field separators.
    # Change Synergy uses \x1C-\x1E in attribute
    # "transition_log" of "problem" objects, so these are out.
    # Also people have been known to enter strange characters
    # like \cG even when using a GUI exclusively.
    my $format = "\cA" . join("\cD", values %wanted) . "\cD";
    my @wanted = keys %wanted;

    my ($rc, $out, $err) = exists $wanted{finduse} ?
	$self->_ccm_with_option(
	    Object_format => $format, qw(finduse -query), $query) :
	$self->_ccm(0, qw(query -u -ns -nf -format), $format, $query);

    # NOTE: if there are no hits, `ccm query' exits with status 1, 
    # but produces no output on either stdout and stderr
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    $out =~ s/\A\cA//;				# trim leading record separator
    foreach my $row (split(/\cA/, $out))	# split into records 
    {
	my ($finduse, %hash);

	# split into columns and translate "<void>" to undef
	(@hash{@wanted}, $finduse) = 
	    map { $_ eq "<void>" ? undef : $_ } split(/\cD/, $row);

	# handle special keywords
	if (exists $wanted{objectname})
	{
	    # Sigh. "ccm query -f %objectname" returns old-style fullnames
	    # (i.e. "instance/cvtype/name/version") for certain types of 
	    # objects, e.g. "cvtype" and "attype". But it will not accept
	    # these later where a "file_spec" is expected (at least on Unix,
	    # because they contain slashes). Hence rewrite these fullnames
	    # to correct objectnames.
	    $hash{objectname} =~ s|^(.*?)/(.*?)/(.*?)/(.*?)$|$3$self->{delimiter}$4:$2:$1|;
	}
	if (exists $wanted{object})
	{
	    # see above
	    $hash{object} =~ s|^(.*?)/(.*?)/(.*?)/(.*?)$|$3$self->{delimiter}$4:$2:$1|;

	    # objectify 'object' column
	    $hash{object} = $self->object($hash{object});
	}
	if (exists $wanted{finduse})
	{
	    # FIXME: for 6.3 we want the full objectname for the project
	    # (maybe 6.3 "ccm finduse..." already returns this form?)

	    # parse finduse list (the last column)
	    $hash{finduse} = {};

	    # NOTE [DEPRECATE 4.5]: `Object not used'
	    unless ($finduse =~ /Object is not used in scope|Object not used/)
	    {
		# lines are of the form
		# \t relative_path/object_name-version@project_name-project_version 
		# which we parse into a hash
		# "project_name-project_version" => "relative_path/object_name"
		while ($finduse =~ /$self->{finduse_rx}/g)
		{
		    $hash{finduse}->{$2} = $1;
		}
	    }
	}

	push @result, $wanthash ? \%hash : [ @hash{@keywords} ];
    }

    return \@result;
}

sub query_object
{
    my ($self, $query) = @_;
    _usage(2, 2, '$query', \@_);

    my $result =  $self->_query($query, 1, qw(object));
    return undef unless $result;

    # slice out the single "object" column
    return [ map { $_->{object} } @$result ];
}



sub history
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'history', @_);
    return $self->set_error($err || $out) unless $rc == 0;

    return [ split(/^\*+\n?/m, $out) ];
}


sub history_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, 0, @keywords);
}

sub history_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    return $self->_history($file_spec, 1, @keywords);
}

# helper: history with correct handling of multi-line attributes
sub _history
{
    my ($self, $file_spec, $wanthash, @keywords) = @_;

    my %wanted = map { ($_, "%$_") } @keywords;
    $wanted{object} = "%objectname"	if exists $wanted{object};
    $wanted{predecessors} = "?"		if exists $wanted{predecessors};
    $wanted{successors} = "?"		if exists $wanted{successors};

    my $format = "\cA" . join("\cD", values %wanted) . "\cD";

    my ($rc, $out, $err) = $self->_ccm(0, qw(history -f), $format, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my @result;
    $out =~ s/\A\cA//;				# trim leading record separator
    foreach my $row (split(/\cA/, $out))	# split into records 
    {
	# split into columns and translate "<void>" to undef
	my @cols = map { $_ eq "<void>" ? undef : $_ } split(/\cD/, $row);

	my ($history, %hash);
	(@hash{keys %wanted}, $history) = @cols;

	# handle special keywords
	if (exists $wanted{objectname})
	{
	    # cf. _query()
	    $hash{objectname} =~ s|^(.*?)/(.*?)/(.*?)/(.*?)$|$3$self->{delimiter}$4:$2:$1|;
	}
	if (exists $wanted{object})
	{
	    # cf. _query()
	    $hash{object} =~ s|^(.*?)/(.*?)/(.*?)/(.*?)$|$3$self->{delimiter}$4:$2:$1|; 
	    # objectify 'object' column
	    $hash{object} = $self->object($hash{object});
	}
	if (exists $wanted{predecessors} || exists $wanted{successors})
	{
	    # parse history (the last column)
	    my ($predecessors, $successors) = $history =~
		/^Predecessors:\n\t?(.*)
		 ^Successors:\n\t?(.*)
		 ^\*
		/msx;

	    if (exists $wanted{predecessors})
	    {
		$hash{predecessors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $predecessors) ];
	    }
	    if (exists $wanted{successors})
	    {
		$hash{successors} = 
		    [ map { $self->object($_) } split(/\n\t?/, $successors) ];
	    }
	}

	push @result, $wanthash ? \%hash : [ @hash{@keywords} ];
    }

    return \@result;
}


sub finduse
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'finduse', @_);

    # NOTE: `ccm finduse ...' without `-query' complains if some of 
    # the given objects do not exist (and exits with status 1 unless at least
    # one exists). But for `ccm finduse -query ...', if there are no hits, 
    # the command exits with status 1 and produces no output on either 
    # stdout and stderr. (This is the same behaviour as for `ccm query ...'.) 
    # We will not produce an error in any case. However, the returned array
    # may contain fewer elements than file_specs given as arguments.

    if ($rc == 0)
    {
	my (@result, $uses);
	foreach (split(/\n/, $out))
	{
	    # ignore complaints about non-existing objects 
	    # and the dummy "use" line printed if object is not used anywhere
	    # NOTE [DEPRECATE 4.5]: `Object not used'
	    next if /Object version could not be identified|Object is not used in scope|Object not used/;

	    # a usage line is matched by finduse_rx
	    $uses->{$2} = $1, next if /$self->{finduse_rx}/;

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
    return undef unless defined $finduse;
    return $self->set_error("`$file_spec´ matches more than one object") unless @$finduse == 1;
    return $finduse->[0]->[1]->{$proj_vers};
}

sub get_attribute
{
    my ($self, $attr_name, $file_spec) = @_;
    _usage(3, 3, '$attr_name, $file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(0, 'attribute', -show => $attr_name, $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;
    return $out;
}


sub set_attribute
{
    my ($self, $attr_name, $file_spec, $value) = @_;
    _usage(4, 4, '$attr_name, $file_spec, $value', \@_);

    # use ye olde text_editor trick if $value may cause problems
    # (depending on execution mode and platform) because its
    # too long or contains unquotable characters or...
    my ($rc, $out, $err);
    if (($self->{coprocess} && (length($value) > 1600 || $value =~ /["\r\n]/)) ||
        ($Is_MSWin32 && (length($value) > 100 || $value =~ /[%<>&"\r\n]/)))
    {
	($rc, $out, $err) = $self->_ccm_with_text_editor($value, 
	    'attribute', -modify => $attr_name, $file_spec);
    }
    else
    {
	($rc, $out, $err) = $self->_ccm(0,
	    'attribute', -modify => $attr_name, -value => $value, $file_spec);
    }
    return $self->set_error($err || $out) unless $rc == 0;
    return $value;
}


sub create_attribute
{
    my ($self, $name, $type, $value, @file_specs) = @_;
    _usage(5, undef, '$name, $type, $value, $file_spec...', \@_);

    my @args = (-type => $type, @file_specs);
    unshift @args, -value => $value if defined $value;
    # FIXME this should employ the same heuristic as set_attribute()
    # and use a separate _ccm_with_text_editor(..., 'attribute -modify', ...)
    # for troublesome $value

    return $self->ccm('attribute', -create => $name, @args);
}

sub delete_attribute
{
    my ($self, $name, @file_specs) = @_;
    _usage(3, undef, '$name, $file_spec...', \@_);

    return $self->ccm('attribute', -delete => $name, @file_specs);
}

sub copy_attribute
{
    my ($self, $name, $flags, $from_file_spec, @to_file_specs) = @_;
    _usage(5, undef, '$name, \%flags, $from_file_spec, $to_file_spec...', \@_);

    $name = join(':', @$name) if ref $name;

    my @args = ($from_file_spec, @to_file_specs);
    unshift @args,  map { "-$_" } @$flags if $flags;

    return $self->ccm('attribute', -copy => $name, @args);
}

sub list_attributes
{
    my ($self, $file_spec) = @_;
    _usage(2, 2, '$file_spec', \@_);

    my ($rc, $out, $err) = $self->_ccm(0, 'attribute', -la => $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my %attrs = $out =~ /^(\S+) \s* \( (.*?) \)/gmx;
    return \%attrs;
}

sub property
{
    my ($self, $keyword, $file_spec) = @_;
    _usage(3, 3, '$keyword, $file_spec', \@_);

    my ($rc, $out, $err) = 
	$self->_ccm(0, qw(properties -nf -format), "%$keyword", $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    return $out eq "<void>" ? undef : $out;
}


sub types
{
    my $self = shift;
    my ($rc, $out, $err) = $self->_ccm(0, qw(show -types));
    return $self->set_error($err || $out) unless $rc == 0;

    return split(/\n/, $out);
}

sub ls
{
    my $self = shift;

    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, 'ls', @_);
    return $self->set_error($err || $out) unless $rc == 0;

    return [ split(/\n/, $out) ];
}

sub ls_object
{
    my ($self, $file_spec) = @_;
    $file_spec = '.' unless defined $file_spec;

    my $rows = $self->ls(qw(-f %objectname), $file_spec);
    return undef unless $rows;
    return [ map { $self->object($_) } @$rows ];
}

sub ls_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    my $ary_ref = $self->ls_hashref($file_spec, @keywords);
    return unless $ary_ref;
 
    return [ map { [ @$_{@keywords} ] } @$ary_ref ];
}

sub ls_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    _usage(3, undef, '$file_spec, $keyword...', \@_);

    my $format = join("\a", map { "%$_" } @keywords);

    my $rows = $self->ls(qw(-f), $format, $file_spec);
    return undef unless $rows;

    my @result;
    foreach (@$rows)
    {
	my %hash;
	@hash{@keywords} = map { $_ eq "<void>" ? undef : $_ } 
			       split(/\a/, $_);

	push(@result, \%hash);
    }
    return \@result;
}

    

sub set
{
    my ($self, $option, $value) = @_;
    _usage(1, 3, '[$option [, $value]]', \@_);

    if (@_ == 1)
    {
	my ($rc, $out, $err) = $self->_ccm(0, 'set');
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
	my ($rc, $out, $err) = $self->_ccm(0, set => $option);
	$out = undef if $rc == 0 &&  $out eq "(unset)";
	return ($rc, $out, $err);
    }

    if (@_ == 3)
    {
	my ($rc, $out, $err) = defined $new_value ?
	    $self->_ccm(0, set => $option, $new_value) :
	    $self->_ccm(0, unset => $option);
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

    my ($rc, $out, $err) = $self->_set($option);
    return ($rc, $out, $err) unless $rc == 0;
    my $old_value = $out;

    ($rc, $out, $err) = $self->_set($option, $new_value);
    return ($rc, $out, $err) unless $rc == 0;

    my ($ccm_rc, $ccm_out, $ccm_err) = $self->_ccm(0, @args);

    ($rc, $out, $err) = $self->_set($option, $old_value);

    return ($ccm_rc, $ccm_out, $ccm_err) if $rc == 0;
    return ($rc, $out, $err);
}

# helper: implements ye olde text_editor trick for ccm commands
# that would interactively open an editor in order to let the user modify
# some (text) value; _ccm_with_text_editor writes $text_value 
# to a temporary file, then calls _ccm_with_option with
# text_editor="cp temporary_file %filename" and returns its results
# calls $self->_ccm(@args).

sub _ccm_with_text_editor
{
    my ($self, $text_value, @args) = @_;

    my $text_file = $self->{text_file};
    unless (defined $text_file)
    {
	(undef, $text_file) = tempfile(SUFFIX => ".dat");
	return _error("can't create temp file to set text value: $!")
	    unless defined $text_file;

	push @{ $self->{files_to_unlink} }, $text_file;
	$self->{text_file} = $text_file;
    }

    local *TEXT;
    open(TEXT, ">", $text_file)
	or return _error("can't open temp file `$text_file' to set text value: $!");
    print TEXT $text_value;
    close(TEXT);

    # NOTE: 
    # (1) $text_file is safe wrt cygwin, because $Config{cp} is
    #     a cygwin program ("/usr/bin/cp") on cygwin.
    # (2) $Config{cp} = "copy" on Win32, but CMSynergy seems unable to
    #     to execute "shell" builtins, hence use "xcopy" instead
    #     (use "/y" to overwite files without prompting)
    return $self->_ccm_with_option(
	text_editor => $^O eq 'MSWin32' ?
	    "xcopy /y /q \"$text_file\" \"%filename\"" :
	    "$Config{cp} '$text_file' '%filename'",
	@args);
}


sub get_releases
{
    my ($self) = @_;

    my ($rc, $out, $err) = $self->_ccm(0, qw(releases -show));
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
    _usage(2, 2, 'hashref', \@_);

    my $text = "";
    {
	local $" = ", ";
	while (my ($release, $names) = each %$releases) 
	{
	    $text .= "$release: @$names\n";
	}
    }

    my ($rc, $out, $err) =
	$self->_ccm_with_text_editor($text, qw(releases -edit));

    return $rc == 0 || $self->set_error($err || $out);
}


sub ps					# class/instance method
{
    my ($this, @filter) = @_;
    my ($rc, $out, $err);

    my @pscmd = qw(ps);
    {
	last unless @filter;

	# Pass the first "field => value" on to `ccm ps´, since 
	# `ccm ps -field value' is usually significantly faster
	# than `ccm ps'.

	# NOTE [DEPRECATE 4.5]: `ccm ps -rfc_address ADDRESS' does not work
	# correctly in Continuus 4.5: it only finds processes
	# if the host part of ADDRESS is given as an IP address (i.e.
	# _not_ as a DNS name) - though `ccm ps' shows rfc addresses 
	# using names (at least if a reverse lookup on the address succeeds).
	# Esp. `ccm ps -rfc_address $CCM_ADDR' will not work in most cases.
	last if $filter[0] eq 'rfc_address' && $this->version < 5.0;

	push @pscmd, "-$filter[0]", $filter[1];
	splice(@filter, 0, 2);
    }
    ($rc, $out, $err) = $this->_ccm(0, @pscmd);
    return $this->set_error($err || $out) unless $rc == 0;

    my @ps = ();
    my $process;
    foreach (split(/\n/, $out))
    {
	if (/^rfc address \((.*?)\)/)
	{
	    $process = { rfc_address => $1 };
	    push @ps, $process;
	    next;
	} 
	next unless defined $process;

	if (/^\tdb:(.*) \(\)/)		# special fields for object registrar
	{
	    push @{ $process->{db} }, $1;
	    next;
	}
	if (/^\t(\S+) \((.*?)\)/)
	{
	    $process->{$1} = $2;
	    next;
	}
    }

    while (my ($field, $value) = splice(@filter, 0, 2))
    {
	@ps = grep { $_->{$field} eq $value } @ps;
    }

    return \@ps;
}

# FIXME does not work on windows 
# (also not on unix clients that don't have ccmdb_server installed)
sub databases					# class/instance method
{
    my ($this, $servername) = @_;

    my @ccmdb_server = 
    (
	File::Spec->catfile($this->ccm_home, qw(bin ccmdb_server)),
	'-status'
    );
    push @ccmdb_server, $servername if defined $servername;

    my ($rc, $out, $err) = 
	_ccmexec(ref $this ? $this->{env} : { CCM_HOME => $this->ccm_home },
	         @ccmdb_server);
    return ref $this ?
	$this->set_error($err || $out) : undef unless $rc == 0;

    # strip leading/trailing stuff
    $out =~ s/\A.*?^===.*?\n(.*?)\n\n.*\Z/$1/ms;
    return grep { !/dbpath not available/ }
          map  { (split(' ', $_, 3))[2]  } split(/\n/, $out);
}



sub version					# class/instance method
{
    my $this = shift;

    my ($short_version, @full_version) = _version($this->ccm_home);
    return wantarray ? @full_version : $short_version;
}

# helper (not a method, hence memoizable): 
# returns full version information
sub _version
{
    my ($ccm_home) = @_;

    local $ENV{CCM_HOME} = $ccm_home;	# because we're going to call a class method

    # FIXME will work, but unclean wrt CCM_HOME
    my ($rc, $out, $err) = __PACKAGE__->_ccm(0, qw(version -all));
    return __PACKAGE__->set_error($err || $out) unless $rc == 0;

    my $cmsynergy_rx = qr/(?:Continuus|CM Synergy)/;
    my ($version, $short_version) = $out =~ /^$cmsynergy_rx Version\s+((\d+\.\d+).*)$/imo;
    
    my ($schema) = $out =~ /^$cmsynergy_rx Schema Version\s+(.*)$/imo;
    my ($informix) = $out =~ /^Informix.* Version\s+(.*)$/imo;
    my @patches;
    @patches = split(/\n/, $1) 
	if $out =~ /^$cmsynergy_rx Patch Version\s+(.*?)(?:\Z|^$cmsynergy_rx|^Informix)/imso; 
    
    return ($short_version, $version, $schema, $informix, @patches);
}

memoize('_version');



sub status					# class/instance method
{
    my $this = shift;
    my ($rc, $out, $err) = $this->_ccm(0, 'status');
    return $this->set_error($err || $out) unless $rc == 0;

    my (@sessions, $session, $user);
    foreach (split(/\n/, $out))
    {
	if (/sessions for user (\S+):/)
	{
	    $user = $1;
	    next;
	}
	if (/^(Graphical|Command) Interface \@ (\S+)( \(current session\))?/)
	{
	    $session = { 
		rfc_address	=> $2,
		process		=> $1 eq "Graphical" ? "gui_interface" : "cmd_interface",
		user		=> $user,
		current		=> defined $3,
	    };
	    push @sessions, $session;
	    next;
	}
	if (/^Database: (.*)/ && $session)
	{
	    # sanitize database path (all other CM Synergy information commands
	    # show it with trailing "/db", so we standardize on that)
	    # NOTE: carefull here, because the database might reside on NT
	    ($session->{database} = $1) 		
		=~ s{^(.)(.*?)(\1db)?$}{$1$2$1db};
	    next;
	}
    }
    return \@sessions;
}

# generic wrapper for undefined method "foo":
# 	$ccm->foo(@args)
# gets turned into
# 	$ccm->ccm("foo", @args)
# in fact, we create a method `foo' on the fly with this definition
#
# FIXME: this should be an optional feature, maybe enabled by some
# 	use VCS::CMSynergy ':wrapper';
sub AUTOLOAD
{
    my ($this) = @_;

    our $AUTOLOAD;

    # NOTE: the fully qualified name of the method has been placed in $AUTOLOAD
    my ($class, $method) = $AUTOLOAD =~ /^(.*)::([^:]*)$/;
    return if $method eq 'DESTROY'; 

    # we don't allow autoload of class methods
    croak "Can't locate class method `$method' via class `$class'"
	unless ref $this;
    $Debug && $this->trace_msg("autoloading method `$method'\n");

    # create the new method on the fly
    no strict 'refs';
    *{$method} = sub 
	{
	    my $self = shift;

	    my ($rc, $out, $err) = $self->_ccm($OneArgFoo && @_ == 1, $method, @_);

	    return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
	    return $self->set_error($err || $out, undef, 0, $rc, $out, $err);
	};

    # call it w/o pushing a new stack frame (with same parameters)
    goto &$method;
}

sub mkInstallation		# FIXME need better name
{
    my ($this, $ccm_home) = @_;

    (my $class = $ccm_home) =~ s/\W/_/g;
    my $package = __PACKAGE__;
    $class = "${package}::${class}";

    unless (defined *{"${class}::"})
    {
	no strict 'refs';

	eval <<"";
	    package $class;
	    use vars '\@ISA';
	    \@ISA = ('$package');

	die "mkInstallation failed: $@" if $@;

	*{"${class}::ccm_home"} = sub
	    {
		my $this = shift;
		return ref $this ? $this->{env}->{CCM_HOME} : $ccm_home;
	    };
    }
    return $class;
}

sub trace
{
    my ($this, $trace_level, $trace_filename) = @_;
    return $Debug unless defined $trace_level;
    ($Debug, $trace_level) = ($trace_level, $Debug);
    if (@_ == 3)				# $trace_filename present
    {
	# switch trace files
	my $newfh = defined $trace_filename ?
	    IO::File->new($trace_filename, "a") : 
	    IO::Handle->new_from_fd(\*STDERR, "w");
	unless ($newfh)
	{
	    carp(__PACKAGE__ . " trace: can't open trace file `$trace_filename'");
	    return $trace_level;
	}
	$newfh->autoflush(1);
	close($Debugfh);
	$Debugfh = $newfh;
	$Debug && $this->trace_msg(__PACKAGE__ . " version $VERSION: trace started\n");
    }
    $Debug && $this->trace_msg("trace level set to $Debug\n");
    return $trace_level;
}

sub trace_msg
{
    my ($this, $message, $min_level) = @_;
    $min_level ||= 1;
    print $Debugfh "[$this] $message" if $Debug >= $min_level;
}



sub set_error 
{
    my ($this, $error, $method, $rv, @rv) = @_;

    $method = (caller(1))[3] unless defined $method;
    $Error = $error;
    return wantarray ? @rv : $rv unless ref $this;	

    $this->{error} = $error;

    # try the HandleError routine if one was provided;
    # consider the error handled if it returns true
    # FIXME: should the handler be allowed to change the value of $rv?
    my $handler = $this->{HandleError};
    return wantarray ? @rv : $rv if $handler and &$handler($error, $this, $rv, @rv);

    my $msg = "$method: $error";
    croak($msg) if $this->{RaiseError};	
    carp($msg)  if $this->{PrintError};
    return wantarray ? @rv : $rv;
}

# test whether session is still alive (without causing an exception)
# FIXME document ping
sub ping
{
    my ($rc) = shift->_ccm(0, 'delimiter');
    return $rc == 0;
}

# $ccm->object(objectname) => VCS::CMSynergy::Object
# $ccm->object(name, version, cvtype, instance) => VCS::CMSynergy::Object
sub object
{
    my $self = shift;

    croak(__PACKAGE__."::object: invalid number of arguments" .
          "\n  usage: \$ccm->(\$name, \$version, \$cvtype, \$instance)" .
          "\n  or     \$ccm->('name-version:cvtype:version')")
	unless @_ == 1 || @_ == 4;
    
    return VCS::CMSynergy::Object->new($self, @_) if @_ == 4;

    return VCS::CMSynergy::Object->new($self, $1, $2, $3, $4)
	if $_[0] =~ /$self->{objectname_rx}/;

    return $self->set_error("invalid objectname `$_'");
}

# $ccm->object_other_version(object, version) => VCS::CMSynergy::Object
#	new Object with same name/cvtype/instance as OBJECT, but version VERSION
sub object_other_version
{
    my ($self, $object, $other_version) = @_;
    return $self->object($object->name, $other_version, $object->cvtype, $object->instance);
}

1;

__END__

=head1 DESCRIPTION

  use VCS::CMSynergy;

  my $ccm = VCS::CMSynergy->new(database => "/ccmdb/test/tut62/db");

  $ccm->checkout(qw(foo/bar.c@foo~user -to test))
    or die "checkout failed: ".$ccm->error;

  my $csrcs = $ccm->query_hashref("type = 'csrc'",
				  qw(displayname modify_time));
  if ($csrcs)
  {
    print "$_->{displayname} $->{modify_time}\n" foreach (@$csrcs);
  }

=head1 METHODS

=head2 new

  my $ccm = VCS::CMSynergy->new( database => "/ccmdb/foo/db" )
              or die VCS::CMSynergy->error;

Starts a new CM Synergy session. Returns a session handle if it succeeds. 

If it fails to start a session, it returns C<undef>. Use
C<< VCS::CMSynergy->error >> to get the error string printed by CM Synergy.

Multiple simultaneous sessions to multiple databases or with
engines running on different hosts, even using different versions
of CM Synergy, are supported.

C<new> issues a B<ccm start> command and remembers the C<CCM_ADDR>
in the session object (together with other session state).
The session is stopped (B<ccm stop>) when the session object
is destroyed (see L</DESTROY>).

C<new> is called with an attribute hash. The following attributes
are currently supported:

=over 4

=item C<database> (string)

CM Synergy database path. 

This is the only attribute required on Unix systems.

=item C<host> (string)

CM Synergy engine host to use.

It defaults to the local host.

=item C<role> (string)

User's initial CM Synergy role.

It defaults to C<developer>.

=item C<user> (string)

CM Synergy user. 

This attribute is available and required on Windows systems only.

=item C<password> (string)

User's password. 

This attribute is required on Windows systems or when using
ESD to connect to the CM Synergy engine.

=item C<ini_file> (string)

CM Synergy ini file to use. 

In contrast to the CM Synergy B<ccm start> command there is I<no>
default ini file consulted. (On Unix systems this is achieved
by executing B<ccm start> with the option C<-f /dev/null>.) The reason
is that we want scripts to behave in a reproducible way. Otherwise
the script might accidentally work with the current contents of
the current user's ini file, but might fail when invoked by another user.
Or it might fail when invoked by the same user at a later time because of
changes to her ini file (e.g. because of another session between
invocations of the script). So if you really want to rely on an ini file,
you have to supply it explicitly.

=item C<CCM_ADDR> (string)

Specifies the RFC address of an established CM Synergy session.

If you specify this attribut L</new> does not create a new session,
but will attach to the one specified. Also, implicitly sets C<KeepSession>
to "on" so that destruction of the new session
handle will not cause a B<ccm stop>. However, setting C<KeepSession> 
explicitly will take precedence.

Note that there is no default value. In particular, L</new> ignores
the environment variable of the same name.

=item C<CCM_HOME> (string)

Value of the C<CCM_HOME> environment variable to use for this session.

It defaults from the environment variable of the same name,
i.e. C<$ENV{CCM_HOME}>.

This is only of interest if you have multiple version of CM Synergy
installed. You can have simultaneous sessions using different
CM Synergy versions (the module takes care of setting the C<CCM_HOME>
variable appropriately before issuing any C<ccm> commands). 

=item C<ui_database_dir> (string)

Specifies the path name to which your database information is copied 
when you are running a remote client session. This corresponds
to the C<-u pathname> option for B<ccm start>.

Note: This option is particularly useful for Windows clients. If L</new>
fails with something like 

  Server Database Path ... is not accessible from this Client.   
  Please specify a Client Database Path

you should specify this option with a local directory path, e.g.

  my $ccm = VCS::CMSynergy->new(..., ui_database_dir => 'c:\\temp', ...);

The value is  what you would enter under 
"Client Information"/"Database Path" in the GUI's "Startup Info" window.
Or you can set B<ui_database_dir> in the [Options] section of 
the system ini file (note that setting it in your personal ini file
won't do, as this file is I<not> read by L</new> by default).

=item C<remote_client> (boolean)

If the value is "on", it specifies that you want to start the CM Synergy
session as a remote client. This corresponds to the C<-rc> option for
B<ccm start>. This option is only usefull on Unix systems. It defaults
to "off".

=item C<PrintError> (boolean)

This attribute can be used to force errors to generate warnings (using
L<carp|Carp/carp>) in addition to returning error codes in the normal way.  
When set to true, any method which results in an error occuring will cause
the corresponding C<< $ccm->error >> to be printed to stderr.

It defaults to "on".

Note: L</PrintError> and L</RaiseError> below are stolen from the excellent
L<DBI> module.

=item C<RaiseError> (boolean)

This attribute can be used to force errors to raise exceptions 
(using L<croak|Carp/croak>) rather than simply return error codes in the normal way. 
When set to true, any method which results in an error will cause
effectively a C<die> with the actual C<< $ccm->error >>
as the message. 

It defaults to "off".

If you turn C<RaiseError> on then you'd normally turn C<PrintError> off.
If C<PrintError> is also on, then the C<PrintError> is done first (naturally).

Typically C<RaiseError> is used in conjunction with C<eval { ... }>
to catch the exception that's been thrown and followed by an
C<if ($@) { ... }> block to handle the caught exception. 

If you want to temporarily turn C<RaiseError> off (inside a library function
that is likely to fail, for example), the recommended way is like this:

  {
    local $ccm->{RaiseError};  # localize and turn off for this block
    ...
  }

The original value will automatically and reliably be restored by Perl,
regardless of how the block is exited.
The same logic applies to other attributes, including C<PrintError>.

=item C<HandleError> (code ref)

This attribute can be used to provide your own
alternative behaviour in case of errors. If set to a
reference to a subroutine then that subroutine is called
when an error is detected (at the same point that
L</RaiseError> and L</PrintError> are handled).

The subroutine is called with three parameters: the
error message string that L</RaiseError> and L</PrintError>
would use, the C<VCS::CMSynergy> object being used, and the 
value being returned by the method that failed (typically undef).

If the subroutine returns a false value then the
L</RaiseError> and/or L</PrintError> attributes are checked
and acted upon as normal. Otherwise the error is considered "handled"
and execution proceeds normally with a return from the method.

For example, to "die" with a full stack trace for any error:

  use Carp;
  $ccm->{HandleError} = sub { confess(shift) };

=item C<KeepSession> (boolean)

If this attribute is "on" then destruction of the new session handle
will not cause a B<ccm stop>. 

This may be used if you want to
create a new CM Synergy session in one program and then re-use it
in another program (since session creation is a rather time consuming
operation). In this case you should use C</ccm_addr> to extract
the session's RFC address (after C</new> returns) and somehow pass it
on to the other program.

It defaults to "off" unless you also specify C<CCM_ADDR>.

=item C<UseCoprocess> (boolean)

This feature is highly experimental, B<use it at your own risk>.

B<You must have the L<Expect> module installed to use this feature.>
(Since L<Expect> is not available for Win32 systems, 
C<UseCoprocess> is ignored there.)

If C<UseCoprocess> is "off", C<VCS::CMSynergy.pm> executes a separate
C<ccm> process whenever it invokes the CM Synergy CLI, e.g.

  $ccm->checkout('foo.c');
  $ccm->set_attribute('color', 'foo.c', 'blue');
  $csources = $ccm->query("name match '*.c'");

results in the execution of the following three processes:

  ccm checkout foo.c
  ccm attribute -modify color -value blue foo.c
  ccm query "name match '*.c'"

In particular, we incur the startup overhead of B<ccm> three times.
This overhead is noticable, esp. if you are doing 
lots of CM Synergy operations.

If C<UseCoprocess> is "on", only one B<ccm> process per CM Synergy
session ever gets executed. The way it works is that
C<< VCS::CMSynergy->new >> starts an "interactive"
(i.e. one invoked without arguments) B<ccm> process in the background.
Later invocations of the CM Synergy CLI pipe their commands to its input and 
read back the output (up to the next C<< "ccm>" >> prompt). 
The actual command is then followed in the same way by C<set error>
to retrieve the success status. Destruction  of the session object
will cause termination of this "coprocess" (via "stop" or "exit" depending
on the setting of L</KeepSession>).

The "coprocess" method avoids the startup overhead, but may run into 
other problems:

=over 4

=item *

The "interactive" B<ccm> imposes stricter limits 
on the length of one CLI command (experimentally put at ~2000 bytes)
than the "batch" B<ccm> (where the limit on the arguments of a process
is typically imposed by the operating system). Moreover, it will
silently truncate the command and not signal an error (unless the
truncation causes a syntax error).

=item *

The current method to communicate with the "coprocess" does not allow
for separation of its stdout and stderr.

=item *

FIXME: chdir problem

=item *

C<UseCoprocess> does not work under Win32 at all.

=back

The default value of C<UseCoprocess> is "off".

=back

=head2 DESTROY

  $ccm->DESTROY;

Stops the CM Synergy session represented by the session handle
by executing B<ccm stop> (unless the session has the C<KeepSession>
attribut set).

You should never call this method explicitly, as it
is invoked by the Perl runtime when the Perl process exits
(either by calling C<exit> or because of a C<die>).
Hence, a script using the C<VCS::CMSynergy> module will not leave
any CM Synergy sessions hanging around. 

Actually, the Perl runtime will call C<DESTROY> when the last reference
to a session handle goes out of scope, so in the following example
each session will be stopped as soon as one loop through the C<foreach>
body is completed, i.e. there is at most one session in progress
at any one time:

  my @databases = ...;		# a list of CM Synergy databases
  foreach my $db (@databases)
  {
    my $ccm = VCS::CMSynergy->new( database => $db, ... );
    ...
    # perform some operation on $db
    ...
    # session is stopped as "my" variable $ccm is about to go out of scope
  }

Note: The correct way to explicitly stop a session is neither

  $ccm->stop;

nor is it

  $ccm->DESTROY;

Though both forms will execute B<ccm stop>,
the first form makes C<$ccm> a C<VCS::CMSynergy> object with an invalid
RFC address (i.e. attribute CCM_ADDR), while the second form leaves you with an
"empty"  C<VCS::CMSynergy> object. Instead, you should rather say

  $ccm = undef;

=head2 ccm

  ($rc, $out, $err) = $ccm->ccm($command, @args);

This is the workhorse of the VCS::CMSynergy module. It executes B<ccm> 
with command C<$command> and (optional) parameters C<@args>.
In array context it returns a three-element array consisting of
the (operating system) exit code of B<ccm>, and what B<ccm> printed
on stdout and stderr. Note that the exit code is 0 if B<ccm>
operated successfully. On DOSish operating systems the 
(possibly multi-line) strings C<$out> and C<$err> have been read
by Perl in "text" mode, i.e. contain LF characters instead of CRLF.
In any case, C<$out> and C<$err> have been C<chomp>ed.

In scalar context C<ccm> returns the
"logical" exit code, i.e. C<!$rc>, so that you can write:

  $ccm->ccm('checkout', $file_spec) 
      or die "checkout failed: ".$ccm->error;

Note that you must pass every C<ccm> argument or option as a single Perl 
argument. For literal arguments the C<qw()> notation may come in handy, e.g.

  ($rc, $out, $err) = $ccm->ccm(qw(finduse -state working));

Most specialized methods in the VCS::CMSynergy module are ultimately implemented
via the L</ccm> method. Using it directly is only recommended for
commands that perform some action, e.g. B<ccm checkout>, as opposed to 
query-like commands. For the latter, e.g. B<ccm query>, use one of the 
methods that return the information in structured form, 
e.g. L</"query_arrayref and query_hashref">, instead of having 
to parse C<$out> yourself.

In fact, there is a shortcut for "action" commands: if you call
a non-existent method on a VCS::CMSynergy object, it tries to invoke
the L</ccm> method with the original method name as the C<$command>
followed by the parameters of the original call, i.e.

  $ccm->checkout($file_spec);

and 

  $ccm->ccm('checkout', $file_spec);

are equivalent (given that there is no real C<checkout> method).
Return values are those of L</ccm> (depending on context).
This is accomplished by a suitable C<AUTOLOAD> method.

=head2 ccm_addr

  print "CCM_ADDR=", $ccm->ccm_addr;

Returns the session's RFC address.

=head2 ccm_command

  $last_session_command = $ccm->ccm_command;
  $last_cmsynergy_command = VCS::CMSynergy->ccm_command;

Returns the last CM Synergy command invoked in the session  or in any
C<VCS::CMSynergy> session, resp.

=head2 ccm_home

  print "CCM_HOME=", $ccm->ccm_home;

Returns the session's CCM_HOME. When invoked as a class method,
simply returns C<$ENV{CCM_HOME}>

=head2 out

Returns the raw standard output of the last CM Synergy command invoked
in the session or in any C<VCS::CMSynergy> session, resp.
In scalar context the output is returned as a possibly multi-line string.
In list context it is returned as an array of pre-chomped lines.

=head2 err

Returns the raw standard error of the last CM Synergy command invoked
in the session or in any C<VCS::CMSynergy> session, resp.
The return value is a possibly multi-line string regardless of context.

=head2 database

  $database = $ccm->database;

Returns the database path in canonical form (i.e. with a trailing C<"/db">):

=head2 delimiter

  $delim = $ccm->delimiter;

Returns the database delimiter.

=head2 error

  $last_session_error = $ccm->error;
  $last_cmsynergy_error = VCS::CMSynergy->error;

Returns the last error that occured in the session or in any
C<VCS::CMSynergy> session, resp.

=head2 query

  $ary_ref = $ccm->query(@args);

Executes the B<ccm query> command with the given C<@args> as parameters.
The output (as formatted by the C<-format> option) is split into lines.
These are L<chomp|perlfunc/chomp>ed and a reference to the resulting array
of strings is returned. 

If there a no hits, a reference to an empty
array is returned. (Note that B<ccm query> considers this an error,
but the VCS::CMSynergy module does not.) 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm query> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.
Example:

  $result = $ccm->query(qw(-t csrc -f), '%displayname %modify_time');
  print "$_\n" foreach (@$result);

If you are interested in the value of several attributes for the
result set of the query, you should look at the 
L</"query_arrayref and query_hashref"> methods that return this information in 
structured form. If you are only interested in the identity of
objects in the result set, you should look at the L</query_object> method.

Note that L</query> will probably produce
unpredictable results when the C<-format> option references attributes
that can have multi-line values, e.g. C<status_log>. 
L</"query_arrayref and query_hashref"> handle this case correctly.

=head2 query_arrayref and query_hashref

  $ary_ref = $ccm->query_arrayref($query, @keywords);
  print "@$_\n" foreach @$ary_ref;

  $ary_ref = $ccm->query_hashref($query, @keywords);
  print "@$_{@keywords}\n" foreach @$ary_ref;

C<query_arrayref> and C<query_hashref>
execute B<ccm query> with the query expression C<$query> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. They both return a reference to an array of references,
one per result row. 

C<query_arrayref> represents a row as an array containing
the values of the keywords for that particular object in the result set
(in the order given by C<@keywords>). 

C<query_hashref> represents a row as a hash containing
attribute and value pairs where the keys are the C<@keywords>.

If the query returned no hits, both C<query_arrayref>
and C<query_hashref> return a reference to an empty array.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array or hash element is C<undef> (whereas B<ccm query> would print it as
the string "C<void>").

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing 
the object in the result set.

=item C<finduse>

The value is a reference to a hash identifying in what parts of what
projects the object is used.  A key in the hash denotes the project
in the form "project_name-project_version".  The hash value is the
corresponding relative path (including the object's name) in the project.
This information is the same as reported by B<ccm finduse>. In fact, if
this keyword is given, L</"query_arrayref and query_hashref"> 
invoke B<ccm finduse -query $query> rather than B<ccm query $query>.  
Example:

  my $result = $ccm->query_arrayref(
    "name = 'main.c'", qw(objectname finduse));

returns (as formatted by L<Data::Dumper>):

  $result = [
    [
      'main.c-1:csrc:3',	# objectname
      {				# finduse
	 'guilib-1.0'	=> 'guilib/sources/main.c',
	 'guilib-int'	=> 'guilib/sources/main.c',
	 'guilib-darcy'	=> 'guilib/sources/main.c'
      }
    ],
    ...
  ];

=back 

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->query_hashref("name match '*.c'", 
                                    qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    print "$row->{displayname} last modified at $row->{modify_time}\n";
    ...
  }

FIXME: document shorthand queries

NOTE: This query method does I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
B<-o owner> or B<-n name>, as these can all be expressed by 
suitable sub clauses of the C<$query> expression.

=head2 query_object

  $ary_ref = $ccm->query_object($query);

Executes B<ccm query> with the query expression C<$query> 
and returns a reference to an array of C<VCS::CMSynergy::Object>s 
that satisfy the query.

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

FIXME: document shorthand queries
NOTE: This query method does I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
B<-o owner> or B<-n name>, as these can all be expressed by 
suitable sub clauses of the C<$query> expression.

NOTE: This is a convenience method. It might be implemented 
using C<query_arrayref>:

  sub query_object
  {
    my ($self, $query) = @_;
    my $ary = $self->query_arrayref($query, 'object') or return undef;
    [ map { $_->[0] } @$ary ];
  }

=head2 history

  $ary_ref = $ccm->history(@args);

Executes the B<ccm history> command with the given C<@args> as parameters.
The output (probably formatted by the C<-format> option) is split into 
chunks at the divider line (a line consisting of lots of asterisks).
A reference to the resulting array of (multi-line) strings is returned. 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm history> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.

If you are interested in the successor or predecessor or 
certain attributes of an object in the history,
you should look at the L</"history_arrayref and history_hashref">
methods that return this information in structured form. 

=head2 history_arrayref and history_hashref

  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

C<history_arrayref> and C<history_hashref>
execute B<ccm history> for C<$file_spec> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. The both return a reference to an array of references,
one per history entry. 

C<history_arrayref> represents a history entry as an array containing
the values of the keywords for that particular object in the history
(in the order given by C<@keywords>). 

C<history_hashref> represents a history entry as a hash
containing attribute and value pairs where the keys are the C<@keywords>.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array or hash element is C<undef> (whereas B<ccm history> would print it as
the string "C<void>").

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing the object in the history.

=item C<predecessors>

The value returned is a reference to an array of C<VCS::CMSynergy::Object>s
that represent the given object's predecessors.

=item C<successors>

The value returned is a reference to an array of C<VCS::CMSynergy::Object>s
that represent the given object's successors.

=back

Note the following differences from B<ccm history>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to get the history
of a project use the full objectname of the project for C<$file_spec>.

=item *

The keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->history_hashref(
    'math.h-1:incl:1', qw(displayname modify_time successors));

  foreach my $row (@$result)
  {
    print "$row->{displayname}: last modified at $row->{modify_time}\n";
    print "\t$_\n" foreach (@{ $row->{successors} });
    ...
  }

=back

=head2 finduse

  $ary_ref = $ccm->finduse(@args);

Executes the B<ccm finduse> command with the given C<@args> as parameters.
It returns a reference to an array of rows, usually one per C<file_spec> given
in C<@args>, or one per query result if C<-query $query_expression>
is present in C<@args>. 

Each row is a reference to an array of two elements.  The first
element is the description of the object.  The second element is a
reference to a hash identifying in what parts of what projects the
object is used.  A key in the hash denotes the project in the form
"project_name-project_version".  The hash value is the corresponding
relative path (including the object's name) in the project.  If there
are no uses of the object in the given scope the hash is empty.  This
usage information is in the same form as that for the pseudo keyword
C<"finduse">  of the  L</"query_arrayref and query_hashref"> methods.

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm finduse> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.

If you are interested in usage information for all objects matching a
query you should look at the L</"query_arrayref and query_hashref">
methods, esp. the C<"finduse"> keyword.

Example (recreate the output of the B<ccm finduse> command):

  foreach (@{ $ccm->finduse(@args) })
  {
    my ($desc, $uses) = @$_;
    print "$desc\n";
    if (keys %$uses)
    {
	while (my ($proj_vers, $path) = each %$uses)
	{
	  print "\t$path\@$proj_vers\n"
	}
    }
    else
    {
	print "\tObject is not used in scope.\n";
    }
  }

=head2 findpath

  $path = $ccm->findpath($file_spec, $proj_vers);

This is a convenience function. It returns the relative pathname
(including the objects's name) for the object C<$file_spec> within the
project C<$proj_vers>.

Returns C<undef> if C<$file_spec> is not used in C<$proj_vers>
or if C<$file_spec> does not exist.

Example:

  $ccm->findpath("main.c-1:csrc:3", "guilib-darcy"); 

returns 

  "guilib/sources/main.c"

=head2 get_attribute

  $value = $ccm->get_attribute($attr_name, $file_spec);

Get the value of the attribute C<$attr_name> for
C<$file_spec> (using (B<ccm attribute -show>). 

If C<RaiseError> is not
set and an error occurs (e.g.  attribute C<$attr_name> does not exist
on object C<$file_spec>), C<undef> will be returned.

Note the following differences from B<ccm attribute -show>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to get an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 set_attribute

  $ccm->set_attribute($attr_name, $file_spec, $value);

Set the value of the attribute C<$attr_name>
for C<$file_spec> to C<$value> (using (B<ccm attribute -modify>).

Returns C<$value> on success.  If C<RaiseError>
is not set and an error occurs (e.g. attribute C<$attr_name> does not
exist on object C<$file_spec>), C<undef> will be returned.

This works for B<all> types of attributes, even those of type I<text>
(or derived from I<text>) and with C<$value>s that may contain
multiple lines or are of arbitrary length.

Note the following differences from B<ccm attribute -modify>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 create_attribute

  $ccm->create_attribute($attr_name, $type, $value, @file_specs);

Create attribute C<$attr_name> of type C<$type> on all objects
given by C<@file_specs> (using B<ccm attribute -create>).
You may also set an initial value
by passing something other than C<undef> for C<$value>.

Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

FIXME: allow multiple attr_names as in copy_attribute?

=head2 delete_attribute

  $ccm->delete_attribute($attr_name, @file_specs);

Delete attribute C<$attr_name> from all objects
given by C<@file_specs> (using B<ccm attribute -delete>).


Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

FIXME: allow multiple attr_names as in copy_attribute?

=head2 copy_attribute

  $ccm->copy_attribute($attr_name, $flags, $from_file_spec, @to_file_specs);

Copy attribute C<$attr_name> from C<$from_file_spec>
by objects given by C<@to_file_specs> (using B<ccm attribute -copy>).

You can specify multiple attributes to copy by passing
a reference to an array of attribute names as C<$attr_name>.

C<$flags> may be C<undef> or a reference to an array containing
a subset of the following strings: C<"append">, C<"subproj">,
C<"suball">, e.g.

  $ccm->copy_attribute($attr_name, [ qw(subproj suball) ], 
  	               "proja-1.0:project:1", "projb-1.0:project:1");

Cf. the CM Synergy documentation on the I<attribute command>
for the meaning of these flags.

Note the following differences from B<ccm attribute -create>:

=over 4

=item *

There is no C<-p> (project) option. If you want to set an attribute 
of a project use the full objectname of the project for C<$file_spec>.

=back

=head2 list_attributes

  $hash_ref = $ccm->list_attributes($file_spec);

Lists all attributes for C<$file_spec> (using B<ccm attribute -la>).

Returns a reference to a hash containing pairs of attribute name
and attribute type (e.g. C<string>, C<time>).
Returns C<undef> in case of error.

Note the following differences from B<ccm attribute -modify>:

=over 4

=item *

Only one C<$file_spec> is allowed.

=back

=head2 property

  $value = $ccm->property($keyword, $file_spec);

Returns the value of property C<$keyword> for C<$file_spec>
(using B<ccm properties -f ...>).
You can use any of the CM Synergy built-in keywords for C<$keyword>.
If the value of C<$keyword> is undefined, C<undef> is returned
(whereas B<ccm properties> would print it as the string "C<void>").

=head2 types

  @types = $ccm->types;

Returns an array of types from B<ccm show -types>.

=head2 ls

  $ary_ref = $ccm->ls(@args);

Executes the B<ccm ls> command with the given C<@args> as parameters.
The output (as formatted by the C<-format> option) is split into lines.
These are L<chomp|perlfunc/chomp>ed and a reference to the resulting array
of strings is returned. 

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm ls> argument or option as a single
Perl argument. 

If you are interested to obtain the value of several attributes,
you should look at the L</ls_arrayref>
and L</ls_hashref> methods that return this information in 
structured form. If you are only interested in the identity of
the listed objects, you should look at the L</ls_object> method.

=head2 ls_object

  $ary_ref = $ccm->ls_object($file_spec);

Lists information about a file or the contents of a directory
using the work area name C<$file_spec>.
Returns a reference to an array of corresponding C<VCS::CMSynergy::Object>s.
The default C<$file_spec> is the working directory.

=head2 ls_arrayref

  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);

Lists the values of the built-in keywords or attributes supplied
in C<@keywords> for a file or the contents of a directory
Returns a reference to an array of references,
one per result row. Each reference points to an array containing
the values of the keywords for that particular object
(in the order given by C<@keywords>). 

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array element is C<undef> (whereas B<ccm ls> would print it as
the string "C<void>").

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->ls('foo', qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    my ($displayname, $type, $modify_time) = @$row;
    print "$displayname ($type) last modified at $modify_time\n";
    ...
  }

=head2 ls_hashref

  $ary_ref = $ccm->ls_hashref($file_spec, @keywords);

Lists the values of the built-in keywords or attributes supplied
in C<@keywords> for a file or the contents of a directory
using the work area name C<$file_spec>.
Returns a reference to an array of references,
one per result row. Each reference points to hash containing
attribute and value pairs where the keys are C<@keywords>.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
hash element is C<undef> (whereas B<ccm ls> would print it as
the string "C<void>").

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->ls_hashref('foo', qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    print "$row->{displayname} last modified at $row->{modify_time}\n";
    ...
  }

=head2 set

  $value = $ccm->set($option);
  $old_value = $ccm->set($option, $new_value);
  $hash_ref = $ccm->set;

Get or sets the value of an option.

In the first form it returns the value of C<$option>. If the option is unset,
C<undef> is returned (whereas B<ccm set> would print C<"(unset)"> in this case).

In the second form, the C<$option> is set to C<$new_value>, the previous
value is returned. If C<$new_value> is C<undef>, C<$option> is unset.

In the third form, a reference to a hash is returned. The hash consists
of all currently defined options as keys and their respective values.

=head2 get_releases and set_releases

  $releases = $ccm->get_releases;
  $ccm->set_releases($releases);

C<get_releases> fetches the release table (of active releases) as printed
by B<ccm releases -show>. It returns a reference to a hash where
each  key is the release name and the value is (a reference to) 
a list of included releases, e.g. as formatted by L<Data::Dumper>:

  $releases = {
      '1.0'	=> [ qw(1.0) ],
      '1.1'	=> [ qw(1.0 1.1) ],
      '2.0'	=> [ qw(1.0 1.1 2.0) ],
      '2.0_SP1'	=> [ qw(1.0 1.1 2.0 2.0_SP1) ],
      '2.1'	=> [ qw(1.0 1.1 2.0 2.1) ],
      '3.0'	=> [ qw(1.0 1.1 2.0 2.1 3.0) ],
      '3.1'	=> [ qw(1.0 1.1 2.0 2.1 3.0 3.1) ]
  };

C<set_releases> updates the release table. It takes a reference to
hash with the same structure as returned by C<get_releases>.

=head2 object

  $obj1 = $ccm->object($objectname);
  $obj2 = $ccm->object($name, $version, $cvtype, $instance);

Create a C<VCS::CMSynergy::Object> from either a objectname
(sometimes called "object reference form" in CM Synergy documentation)
in "name-version:cvtype:instance" format or the four parts specified
separately. 

This is just a wrapper for L<VCS::CMSynergy::Object/new>.
However, when called with one argument, it will also accept the
deprecated fullname in "instance/cvtype/name/version" format.
This comes in handy when creating C<VCS::CMSynergy::Object>s from
query results using %objectname in the query format, as B<ccm query>
actually returns the fullname (instead of the objectname) for certain
base and model objects.

Note that no check is made whether the specified object really exists
in the database.

=head2 ps

  $ary_ref = $ccm->ps;
  $ary_ref = $ccm->ps(user => "jdoe", process => "gui_interface", ...);

Executes B<ccm ps> and returns a reference to an array of references,
one per CM Synergy process. Each reference points to a hash
containing pairs of field names (e.g. C<host>, C<database>, C<pid>) and values
for that particular process as listed by B<ccm ps>.

The available keys vary with the type of the process
(e.g. C<engine>, C<gui_interface>). The process type is listed 
under key C<process>.  The key C<rfc_address> is always present.
The object registrar (i.e. the unique process with key C<process>
equal to "objreg") has a special key C<db>.
Its value is a reference to an array of database names
that the registrar as encountered during its lifetime.

In the second form of invocation, you can pass pairs of field name
and field value and C<ps> will only return processes whose fields
match I<all> the corresponding values. Note that in contrast to the
B<ccm ps> command, you can filter on multiple fields simultaneously.

This method can also be called as a class method,
i.e. C<< VCS::CMSynergy->ps >>, as it does not need
an established session.

Here's an example of the value returned by C<ps> 
as formatted by L<Data::Dumper>:

  $ps = [
      {
	'process' => 'router',
	'host' => 'tiv01',
	'rfc_address' => 'tiv01:5415:160.50.76.15',
	'user' => 'ccm_root',
	'host_addr' => '',
	'pid' => '9428'
      },
      {
	'process' => 'gui_interface',
	'database' => '/ccmdb/tbd/slc/db',
	'engine_address' => 'tiv01:60682:160.50.76.15',
	'host' => 'lapis',
	'user' => 'q076273',
	'msg_handler_1' => 'uissys:message_handler',
	'display' => '',
	'callback' => 'vistartup:cb_init',
	'rfc_address' => 'lapis:1934:160.50.136.36',
	'pid' => '224',
	'host_addr' => ''
      },
      {
	'process' => 'cmd_interface',
	'database' => '/ccmdb/tbd/nasa_ix/db',
	'engine_address' => 'nasaora:1559:160.48.78.33',
	'host' => 'nasaora',
	'user' => 'qx06322',
	'msg_handler_1' => 'uissys:message_handler',
	'display' => 'nasaix11:0',
	'callback' => 'ciserver:cb_init',
	'rfc_address' => 'nasaora:1556:160.48.78.33',
	'pid' => '24367',
	'host_addr' => ''
      },
      {
	'process' => 'engine',
	'database' => '/ccmdb/tbd/nasa_ix/db',
	'host' => 'nasaora',
	'user' => 'qx06322',
	'callback' => 'engine_startup:cb_init',
	'rfc_address' => 'nasaora:1559:160.48.78.33',
	'pid' => '24490',
	'host_addr' => '',
	'ui_address' => 'nasaora:1556:160.48.78.33'
      },
      {
	'process' => 'objreg',
	'db' => [
		  '/ccmdb/tbd/nasa_ix/db',
		  '/ccmdb/tbd/slc/db',
		  '/ccmdb/tbd/eai/db',
		],
	'max_conns' => '256',
	'objreg_machine_addr' => '160.50.76.15',
	'host' => 'tiv01',
	'user' => 'ccm_root',
	'callback' => 'objreg:cb_init',
	'policy' => 'one_per_db',
	'noblock' => 'true',
	'rfc_address' => 'tiv01:60352:160.50.76.15',
	'objreg_machine' => 'tiv01',
	'host_addr' => '',
	'pid' => '9896',
	'objreg_machine_hostname' => 'tiv01'
      },
      ...
  ];

=head2 databases

  @databases = $ccm->databases;
  @databases = $ccm->databases($servername);

Returns an array containing the names of all known CM Synergy databases. 

This method can also be called as a class method,
i.e. C<< VCS::CMSynergy->databases >>,
as it does not need an established session.

=head2 version

  ($ccm, $schema, $informix, @patches) = $ccm->version;

Returns version info about the CM Synergy installation.
In a scalar context C<version> returns the (short) CM Synergy version number.
In an array context the following information is returned:

=over 4

=item *

the full CM Synergy version 

=item *

the database schema version

=item *

the Informix version

=item *

a possible empty array of applied CM Synergy patches

=back

This method can also be called as a class method,
i.e. C<< VCS::CMSynergy->version >> 
as it does not need an established session.

=head2 status

  $ary_ref = $ccm->status;

Executes B<ccm status> and returns a reference to an array of references,
one per CM Synergy session. Each reference points to a hash
containing pairs of field names (e.g. C<database>) and values
for that particular session.

The available keys are a subset of the keys returned by the
L</ps> method: C<rfc_address>, C<database>, C<user>, and C<process>.
There is an additional key C<current> with a boolean value
marking CM Synergy's notion of the I<current> session.

Note: Unlike the output of the B<ccm status> command, the value
for C<database> has a trailing C<"/db">. This makes it consistent
with the session attribute C<database> and the return value of L</ps>.

This method can also be called as a class method,
i.e. C<< VCS::CMSynergy->status >>, as it does not need
an established session.

Here's an example of the value returned by C<status> 
as formatted by L<Data::Dumper>:

  $status = [
      {
	'process' => 'gui_interface',
	'database' => '/ccmdb/scm/support/db',
	'current' => '1',
	'rfc_address' => 'tiv01:53020:160.50.76.15',
	'user' => 'qx06959'
      },
      {
	'process' => 'gui_interface',
	'database' => '/ccmdb/scm/support/db',
	'current' => '',
	'rfc_address' => 'wmuc111931:4661:160.50.136.201',
	'user' => 'qx06959'
      },
      {
	'process' => 'cmd_interface',
	'database' => '/ccmdb/test/tut51/db',
	'current' => '',
	'rfc_address' => 'tiv01:53341:160.50.76.15',
	'user' => 'qx06959'
      }
  ];


=head2 trace

  VCS::CMSynergy->trace($trace_level);
  VCS::CMSynergy->trace($trace_level, $trace_filename);

This class method enables trace information to be written.

Trace levels C<$trace_level> are as follows:

  0 - Trace disabled.
  1 - Trace VCS::CMSynergy method calls.
  2 and above: FIXME

Initially trace output is written to C<STDERR>.  If C<$trace_filename> is
specified and can be opened in append mode then all trace
output is redirected to that file. 
A warning is generated irfs the file can't be opened.
Further calls to C<trace> without a C<$trace_filename> do not alter where
the trace output is sent. If C<$trace_filename> is undefined, then
trace output is sent to C<STDERR> and the previous trace file is closed.

The C<trace> method returns the I<previous> tracelevel.

See also L</trace_msg>.

You can also enable the same trace information by setting the 
C<CMSYNERGY_TRACE> environment variable before starting Perl.

On Unix-like systems using a Bourne-like shell, you can do this easily
on the command line:

  CMSYNERGY_TRACE=2 perl your_test_script.pl

If C<CMSYNERGY_TRACE> is set to a non-numeric value, then it is assumed to
be a file name and the trace level will be set to 2 with all trace
output appended to that file. If the name begins with a number
followed by an equal sign (C<=>), then the number and the equal sign are
stripped off from the name, and the number is used to set the trace
level. For example:

  CMSYNERGY_TRACE=1=trace.log perl your_test_script.pl

=head2 trace_msg

  $ccm->trace_msg($message_text);
  $ccm->trace_msg($message_text, $min_level);

Writes C<$message_text> to the trace file if trace is enabled.
Can also be called as C<< VCS::CMSynergy->trace_msg($msg) >>.
See L</trace>.

If C<$min_level> is defined, then the message is output only if the trace
level is equal to or greater than that level. C<$min_level> defaults to 1.

=head2 set_error

  $ccm->set_error($error);
  $ccm->set_error($error, $method);
  $ccm->set_error($error, $method, $rv, @rv);

Set the L</error> value for the session to C<$error>.
This will trigger the normal DBI error handling
mechanisms, such as L</RaiseError> and L</HandleError>, if
they are enabled.  This method is typically only used internally.

The C<$method> parameter provides an alternate method name
for the L</RaiseError>/L</PrintError> error string.
Normally the method name is deduced from C<caller(1)>.

The L</set_error> method normally returns C<undef>.  The C<$rv>and C<@rv>
parameters provides an alternate return value if L</set_error> was
called in scalar or in list context, resp.

=head1 TODO

=over 4

=item *

anything else?

=back 

=head1 SEE ALSO

L<VCS::CMSynergy::Users>

=head1 AUTHORS

Roderich Schupp, argumentum GmbH <schupp@argumentum.de>

=head1 COPYRIGHT AND LICENSE

The VCS::CMSynergy module is Copyright (c) 2001-2003 argumentum GmbH, 
L<http://www.argumentum.de>.  All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

