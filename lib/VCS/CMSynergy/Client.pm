package VCS::CMSynergy::Client;

our $VERSION = sprintf("%d.%02d", q%version: 1.16 % =~ /(\d+)\.(\d+)/);

use 5.006_000;				# i.e. v5.6.0
use strict;

use Carp;
use Config;
use Cwd;
use File::Spec;
use IPC::Open3;
use POSIX qw(_exit);

# Unix only
use IO::Handle;
use IO::Select;
use IO::File;
use IO::Pipe;				# make ActiveState PerlApp happy

our $Is_MSWin32 = $^O eq 'MSWin32' || $^O eq 'cygwin';
our ($Debug, $Debugfh, $Error, $Ccm_command, $OneArgFoo, $Default);

use Exporter();
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
    $Is_MSWin32 $Debug $Error $Ccm_command $OneArgFoo %new_opts 
    _exitstatus _error _usage);

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
}

our %new_opts = 
(
    HandleError		=> undef,
    PrintError		=> undef,
    RaiseError		=> undef,
    CCM_HOME		=> undef,
);


sub new
{
    my ($class, %args) = @_;
    $class = ref $class if ref $class;

    my $self = 
    {
	HandleError	=> undef,
	PrintError	=> 1,
	RaiseError	=> 0,
	CCM_HOME	=> $ENV{CCM_HOME},
	env		=> {},
	ccm_command	=> undef,
	error		=> undef,
	out		=> undef,
	err		=> undef,
    };	
    bless $self, $class;

    while (my ($arg, $value) = each %args)
    {
	return $self->set_error("unrecognized attribute `$arg'") 
	    unless exists $new_opts{$arg};

	$self->{$arg} = $value;
    }

    $self->{env}->{CCM_HOME} = delete $self->{CCM_HOME};

    return $self;
}


sub start
{
    my ($this, %args) = @_;
    $this = __PACKAGE__->default unless ref $this;

    return VCS::CMSynergy->_start($this, %args);
}


sub default
{
    $Default ||= shift->new();			# CCM_HOME from environment
}


sub ccm						# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;

    my ($rc, $out, $err) = $this->_ccm($OneArgFoo && @_ == 1, @_);

    return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
    return $this->set_error($err || $out, undef, 0, $rc, $out, $err);
    # NOTE: most failing ccm commands issue there error messages on stdout!
}

my $ccm_prompt = qr/^ccm> /;		# NOTE the trailing blank

# helper: just do it (TM), returns ($rc, $out, $err) (regardless of context)
sub _ccm
{
    my ($this, $oneargfoo, $cmd, @args) = @_;
    $Error = $this->{error} = undef;
    $Ccm_command = $this->{ccm_command} = join(" ", $cmd, @args);

    my ($rc, $out, $err);

    USE_COPROCESS:
    {
	last USE_COPROCESS unless $this->{coprocess};

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

	($match, $err, undef, $out, undef) =
	    $this->{coprocess}->expect(undef, -re => $ccm_prompt);
	return _error("expect error: $err") unless $match;

	# on Windows, treat output as if read in "text" mode
	$out =~ s/\015\012/\012/g if $Is_MSWin32;
	chomp($out);

	$this->{coprocess}->print("set error\n");
	($match, $err, undef, $set, undef) =
	    $this->{coprocess}->expect(undef, -re => $ccm_prompt);
	return _error("expect error: $err") unless $match;
	return _error("unrecognized result from `set error': $set")
	    unless ($rc) = $set =~ /^(\d+)/;
	($rc, $err) = (_exitstatus($rc), "");

	$Debug && $this->trace_msg(
	    "<- ccm($this->{ccm_command}) = " .
	    ($Debug >= 8 ? "($rc, `$out', `$err')" : $rc==0) .  "\n");

	($this->{out}, $this->{err}) = ($out, $err);

	return ($rc, $out, $err);
    }

    # simple ccm sub process
    my @exec_args = ($this->ccm_exe, $cmd, @args);
    ($rc, $out, $err) = $this->exec($oneargfoo ?  join(" ", @exec_args) : @exec_args);
    $Debug && $this->trace_msg(
	"<- ccm($this->{ccm_command}) = " .
        ($Debug >= 8 ? "($rc, `$out', `$err')" : $rc==0) .  "\n");

    ($this->{out}, $this->{err}) = ($out, $err);
    return ($rc, $out, $err);
}

sub _spawn_coprocess
{
    my $self = shift;

    eval { require Expect; import Expect 1.15; };
    $Error = $self->{error} = $@, return undef if $@;

    my $env = $self->{env};
    local @ENV{keys %$env} = values %$env if defined $env;

    my $exp = Expect->new
	or $Error = $self->{error} = "Expect->new failed", return undef;
    ($exp->log_stdout(0) && $exp->slave->set_raw && $exp->set_raw)
	or $Error = $self->{error} = $exp->exp_error, return undef;
    $exp->spawn($self->ccm_exe)
	or $Error = $self->{error} = $exp->exp_error, return undef;
    
    # look for initial "ccm> " prompt
    $exp->expect(undef, -re => $ccm_prompt)
	or $Error = $self->{error} = $exp->exp_error, return undef;

    return $exp;
}

sub _kill_coprocess
{
    my $self = shift;
    $self->{coprocess}->print("exit\n");
    # FIXME: kill it just for paranoia (must save pid before line above!)
    $self->{coprocess} = undef;
}

# helper: execute a program with CM Synergy environment set up appropriately
sub exec
{
    my ($this, $prog, @args) = @_;
    $this = __PACKAGE__->default unless ref $this;

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
    # FIXME: add NOTE why we do this (SIGCHLD probs eg with RPC::PlServer)
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
	local @ENV{keys %{ $this->{env} }} = values %{ $this->{env} };
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
	
	carp(__PACKAGE__ . " exec: don't know how to call SIGCHLD handler of type " . ref $_);
    }

    return ($rc, $out, $err);
}

# helper: return pathname to ccm executable
sub ccm_exe
{
    my $self = shift;
    return $self->{ccm_exe} ||= 
	File::Spec->catfile($self->ccm_home, qw(bin ccm));
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

sub error
{
    my $this = shift;
    return ref $this ? $this->{error} : $Error;
}

sub ccm_command
{
    my $this = shift;
    return ref $this ? $this->{ccm_command} : $Ccm_command;
}

sub ccm_home					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;
    return $this->{env}->{CCM_HOME};
}

sub out 					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;
    return wantarray ? split(/\n/, $this->{out}) : $this->{out};
}

sub err 					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;
    return $this->{err};
}

sub version					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;

    our %Version;				# cache by CCM_HOME
    my $ccm_home = $this->ccm_home;
    unless (exists $Version{$ccm_home})
    {
        # "version" is not a recognized "interactive" command
	local $this->{coprocess} = undef;

	my ($rc, $out, $err) = $this->_ccm(0, qw(version -all));
	return $this->set_error($err || $out) unless $rc == 0;

	my %version;
	my $cmsynergy_rx = qr/(?:Continuus|CM Synergy)/;
	($version{cmsynergy}) = $out =~ /^$cmsynergy_rx Version\s+(\S*)$/imo;
	($version{short}) = $version{cmsynergy} =~ /^(\d+\.\d+)/;
	
	($version{schema}) = $out =~ /^$cmsynergy_rx Schema Version\s+(.*)$/imo;
	($version{informix}) = $out =~ /^Informix.* Version\s+(.*)$/imo;
	$version{patches} = [ split(/\n/, $1) ]
	    if $out =~ /^$cmsynergy_rx Patch Version\s+(.*?)(?:\Z|^$cmsynergy_rx|^Informix)/imso; 
	$Version{$ccm_home} = \%version;
    }

    return $Version{$ccm_home}->{short} unless wantarray;
    return @{ $Version{$ccm_home} }{qw(cmsynergy schema informix patches)};
}

sub ps	
{
    my ($this, @filter) = @_;
    $this = __PACKAGE__->default unless ref $this;

    # "ps" is not a recognized "interactive" command
    local $this->{coprocess} = undef;

    my @pscmd = qw(ps);
    if (@filter)
    {
	# Pass the first "field => value" on to `ccm ps´, since 
	# `ccm ps -field value' is usually significantly faster
	# than `ccm ps'.

	# NOTE [DEPRECATE 4.5]: `ccm ps -rfc_address ADDRESS' does not work
	# correctly in Continuus 4.5: it only finds processes
	# if the host part of ADDRESS is given as an IP address (i.e.
	# _not_ as a DNS name) - though `ccm ps' shows rfc addresses 
	# using names (at least if a reverse lookup on the address succeeds).
	# Esp. `ccm ps -rfc_address $CCM_ADDR' will not work in most cases.
	unless ($filter[0] eq 'rfc_address' && $this->version < 5.0)
	{
	    push @pscmd, "-$filter[0]", $filter[1];
	    splice(@filter, 0, 2);
	}
    }

    my ($rc, $out, $err) = $this->_ccm(0, @pscmd);
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


sub status	
{
    my $this = shift;
    $this = __PACKAGE__->default unless ref $this;

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


# FIXME does not work on windows 
# (also not on unix clients that don't have ccmdb_server installed)
sub databases	
{
    my ($this, $servername) = @_;
    $this = __PACKAGE__->default unless ref $this;

    my @ccmdb_server = 
	(File::Spec->catfile($this->ccm_home, qw(bin ccmdb_server)), '-status');
    push @ccmdb_server, $servername if defined $servername;

    my ($rc, $out, $err) = $this->exec(@ccmdb_server);
    return $this->set_error($err || $out) unless $rc == 0;

    # strip leading/trailing stuff
    $out =~ s/\A.*?^===.*?\n(.*?)\n\n.*\Z/$1/ms;
    return grep { !/dbpath not available/ }
          map  { (split(' ', $_, 3))[2]  } split(/\n/, $out);
}

# FIXME does not work on windows 
sub hostname
{
    my ($this, @filter) = @_;
    $this = __PACKAGE__->default unless ref $this;

    our %Hostname;				# cache by CCM_HOME
    my $ccm_home = $this->ccm_home;
    unless (exists $Hostname{$ccm_home})
    {
	my ($rc, $out, $err) = $this->exec(File::Spec->catfile($ccm_home, qw(bin util ccm_hostname)));
        # ignore bogus exit code (seems to be length of output in bytes, arghh)
	$Hostname{$ccm_home} = $out;
    }

    return $Hostname{$ccm_home};
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

    $Error = $this->{error} = $error;

    # try the HandleError routine if one was provided;
    # consider the error handled if it returns true
    my $handler = $this->{HandleError};
    return wantarray ? @rv : $rv if $handler and &$handler($error, $this, $rv, @rv);

    my $msg = "$method: $error";
    croak($msg) if $this->{RaiseError};	
    carp($msg)  if $this->{PrintError};
    return wantarray ? @rv : $rv;
}

1;

__END__


