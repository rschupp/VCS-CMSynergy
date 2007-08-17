package VCS::CMSynergy::Client;

our $VERSION = sprintf("%d.%02d", q%version: 8 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy::Client - base class for CM Synergy methods that don't require a session

=head1 SYNOPSIS

  use VCS::CMSynergy::Client;

  $client = VCS::CMSynergy::Client->new(%attr);

  $ary_ref = $client->ps;
  $ccm_version = $client->version;
  @ary = $client->status;

  $client->trace(1, "trace.out");
  $client->trace_msg("now tracing ccm calls...\n");

  @ary = $client->databases;
  @ary = $client->hostname;

This synopsis only lists the major methods.

=cut

use 5.006_000;					# i.e. v5.6.0
use strict;

use Carp;
use Config;
use Cwd;
use File::Spec;
use IPC::Run3;

# Unix only
use IO::Handle;
use IO::Select;
use IO::File;
use IO::Pipe;					# make ActiveState PerlApp happy

use constant is_win32 => $^O eq 'MSWin32' || $^O eq 'cygwin';
our ($Debug, $Debugfh, $Error, $Ccm_command, $OneArgFoo, $Default);

use Exporter();
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
    is_win32 $Debug $Error $Ccm_command $OneArgFoo %new_opts 
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

    return $self->set_error("CCM_HOME is not set (neither as parameter to new() nor in environment")
	unless defined $self->{CCM_HOME};
    $self->{env}->{CCM_HOME} = delete $self->{CCM_HOME};
    return $self->set_error("CCM_HOME = `$self->{env}->{CCM_HOME}' does not point to a valid CM Synergy installation")
	unless -x $self->ccm_exe || ($^O eq 'cygwin' && -e $self->ccm_exe);
	# NOTE: -x $ccm_exe fails on cygwin

    return $self;
}


sub memoize_method 
{
    my ($class, $method, $slot) = @_;
    $slot ||= $method;

    no strict 'refs';
    no warnings 'redefine';
    my $name = $class . '::' . $method;
    my $coderef = *{$name}{CODE};
    *{$name} = sub 
    {
	my $self = shift;
	$self->{$slot} = &$coderef($self, @_) unless exists $self->{$slot};
	return $self->{$slot};
    };
}


sub start
{
    my ($this, %args) = @_;
    $this = __PACKAGE__->default unless ref $this;

    return VCS::CMSynergy->_start($this, %args);
}


sub default	{ $Default ||= shift->new(); }


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
    my $t0 = $Debug && [ Time::HiRes::gettimeofday() ];

    CCM:
    {
	if ($this->{coprocess})
	{
	    USE_COPROCESS:
	    {
		# arguments cannot contain newlines in "interactive" ccm sessions
		last USE_COPROCESS if grep { /\n/ } @args;

		# FIXME: calling getcwd for every _ccm may be expensive
		if ($this->{cwd} ne (my $cwd = getcwd()))
		{
		    # working directory has changed since coprocess was spawned:
		    # shut down coprocess and start a new one
		    # NOTE: don't call _ccm here (infinite recursion)
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
		$out =~ s/\015\012/\012/g if is_win32;
		chomp($out);

		$this->{coprocess}->print("set error\n");
		($match, $err, undef, $set, undef) =
		    $this->{coprocess}->expect(undef, -re => $ccm_prompt);
		return _error("expect error: $err") unless $match;
		return _error("unrecognized result from `set error': $set")
		    unless ($rc) = $set =~ /^(\d+)/;
		($rc, $err) = (_exitstatus($rc), "");
		last CCM;
	    }
	}

	# simple ccm sub process
	my @exec_args = ($this->ccm_exe, $cmd, @args);
	($rc, $out, $err) = $this->exec($oneargfoo ?  join(" ", @exec_args) : @exec_args);
    }

    if ($Debug)
    {
	my $elapsed = sprintf("%.2f", Time::HiRes::tv_interval($t0));
	if ($Debug >= 8)
	{
	    $this->trace_msg("<- ccm($this->{ccm_command})\n");
	    $this->trace_msg("-> rc = $rc [$elapsed sec]\n");
	    $this->trace_msg("-> out = \"$out\"\n");
	    $this->trace_msg("-> err = \"$err\"\n");
	}
	else
	{
	    my $success = $rc == 0 ? 1 : 0;
	    $this->trace_msg("ccm($this->{ccm_command}) = $success [$elapsed sec]\n");
	}
    }

    ($this->{out}, $this->{err}) = ($out, $err);
    return ($rc, $out, $err);
}

sub _spawn_coprocess
{
    my $self = shift;

    unless (eval "use Expect 1.15; 1")
    {
	$Error = $self->{error} = $@;
	return undef;
    }

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
    my ($this, @cmd) = @_;
    $this = __PACKAGE__->default unless ref $this;

    local @ENV{keys %{ $this->{env} }} = values %{ $this->{env} };

    # don't screw up global $? (e.g. when being called 
    # in VCS::CMSynergy::DESTROY at program termination)
    local $?;			

    my ($out, $err);

    run3(\@cmd, \undef, \$out, \$err, 
	 { binmode_stdout => 1, binmode_stderr => 1 });

    if (my $sig = $? & 127)
    {
	($out, $err) = ("", "Killed by signal $sig");
	$err .= " (core dumped)" if $? & 128;
    }
    else
    {
	$out =~ s/\015\012/\012/g if is_win32; 	# as if read in :crlf mode
	chomp($out, $err);
    }

    return ($?, $out, $err);
}

# helper: return pathname to ccm executable
sub ccm_exe
{
    return File::Spec->catfile(shift->ccm_home, "bin", "ccm$Config{_exe}");
}
__PACKAGE__->memoize_method('ccm_exe');

# helper: inverse function of POSIX::WEXITSTATUS()
sub _exitstatus	{ return $_[0] << 8; }

# helper: return a triple ($rc, $out, $err)
sub _error	{ return (_exitstatus(255), "", $_[0]) }

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

    my $version = $this->_version;
    return @$version{qw(cmsynergy schema informix patches)} if wantarray;
    return $version->{short};
}

sub _version
{
    my $this = shift;

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
    return \%version;
}
__PACKAGE__->memoize_method('_version', 'version');


sub ps	
{
    my ($this, @filter) = @_;
    $this = __PACKAGE__->default unless ref $this;

    # "ps" is not a recognized "interactive" command
    local $this->{coprocess} = undef;

    my ($rc, $out, $err) = $this->_ccm(0, ps => @filter);
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
	if (/sessions for user (\S+):/i)
	{
	    $user = $1;
	    next;
	}
	if (my ($interface, $rfc_address) = /^(.*?) interface \@ (\S+)/i)
	{
	    # start of a session description;
	    # convert interface to process name used by `ccm ps'
	    $session = 
	    {
		process		=> $interface =~ /graphical/i ? 
				     "gui_interface" : "cmd_interface",
		rfc_address	=> $rfc_address,
		user		=> $user,
	    };
	    push @sessions, $session;
	    next;
	}
	if (/^database: (.*)/i && $session)
	{
	    # sanitize database path (all other CM Synergy information commands
	    # show it with trailing "/db", so we standardize on that)
	    # NOTE: careful here, because the database might reside on Windows
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
    my ($list) = $out =~ /^===.*?$(.*?)^There is a total/ms;
    return $this->set_error("unrecognized output from \"ccmdb_server -status\": $out")
	unless defined $list;
    return grep { !/dbpath not available/ }
           map  { (split(' ', $_, 3))[2]  } 
	   split(/\n/, $list);
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

    require Time::HiRes;
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
	$Debug && $this->trace_msg(__PACKAGE__ . " version $VERSION [$^O]: trace started\n");
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

=head1 DESCRIPTION

In most cases there is no need to know about C<VCS::CMSynergy::Client>,
the base class of L<VCS::CMSynergy>.
If you have an established session, you can
invoke all methods on the session object. If you want to use a method
without a session (e.g. L</ps>), invoke it as a class method:

  $ps = VCS::CMSynergy->ps;

You need to use C<VCS::CMSynergy::Client> explicitly if

=over 4

=item *

you want to use a method without a session I<and>

=item *

you have several installations of CM Synergy, i.e. several C<$CCM_HOME>s, I<and>

=item *

you want to switch between different C<$CCM_HOME>s in the same 
invocation of your program.

=back

A typical example is an administrative program that iterates over all 
your CM Synergy databases in all your installations:

  foreach my $ccm_home (qw(/usr/local/ccm51 /usr/local/ccm62 /usr/local/ccm63))
  {
      print "installation in $ccm_home ...\n";
      my $client = VCS::CMSynergy::Client->new(CCM_HOME => $ccm_home);

      foreach my $db ($client->databases)
      {
	  ...
      }
  }

All methods below (except C<new>) can be invoked on either:

=over 4

=item *

a C<VCS::CMSynergy::Client> object

=item *

a C<VCS::CMSynergy> object

=item *

the C<VCS::CMSynergy::Client> class

=item *

the C<VCS::CMSynergy> class

=back

The former two always use the setting of C<CCM_HOME> given at their creation,
while the latter two actually operate on a "default" instance of  C<VCS::CMSynergy::Client>.
This instance is created the first time 
any C<VCS::CMSynergy::Client> or C<VCS::CMSynergy> class method is invoked 
in the course of your program. Its C<CCM_HOME> uses
the value of C<$ENV{CCM_HOME}> that was in effect at the time
the default instance was created. 

=head1 METHODS

=head2 new

  my $client = VCS::CMSynergy::Client->new( CCM_HOME => "/usr/local/ccm62" );

Creates a new CM Synergy client.

If it fails (e.g. CCM_HOME doesn't seem to contain a valid
CM Synergy installation), it returns C<undef>.

C<new> is called with an attribute hash. The following attributes
are currently supported:

=over 4

=item C<CCM_HOME> (string)

Value of the C<CCM_HOME> environment variable to use for this client.

It defaults from the environment variable of the same name,
i.e. C<$ENV{CCM_HOME}>.

=item C<PrintError> (boolean)

This attribute can be used to force errors to generate warnings (using
L<carp|Carp/carp>) in addition to returning error codes in the normal way.  
When set to true, any method which results in an error occuring will cause
the corresponding C<< $ccm->error >> to be printed to stderr.

It defaults to "on".

=item C<RaiseError> (boolean)

This attribute can be used to force errors to raise exceptions 
(using L<croak|Carp/croak>) rather than simply return error codes 
in the normal way. 
When set to true, any method which results in an error will cause
effectively a C<die> with the actual C<< $ccm->error >>
as the message. 

It defaults to "off".

=item C<HandleError> (code ref)

This attribute can be used to provide your own
alternative behaviour in case of errors. If set to a
reference to a subroutine then that subroutine is called
when an error is detected (at the same point that
L</RaiseError> and L</PrintError> are handled).

See the L<< VCS::CMSynergy/C<HandleError> (code ref) >> for details.

=back

=head2 ccm_home

  print "CCM_HOME=", $client->ccm_home;

Returns the setting of CCM_HOME as used by the client.

=head2 error

  $last_error = $client->error;

Returns the last error that occured in this client.

=head2 ccm_command

  $last_cmsynergy_command = $client->ccm_command;

Returns the last CM Synergy command invoked on behalf of the
C<VCS::CMSynergy::Client>.

=head2 out

Returns the raw standard output of the last CM Synergy command invoked
on behalf of the C<VCS::CMSynergy::Client>.
In scalar context the output is returned as a possibly multi-line string.
In list context it is returned as an array of pre-chomped lines.

=head2 err

Returns the raw standard error of the last CM Synergy command invoked
on behalf of the C<VCS::CMSynergy::Client>.
The return value is a possibly multi-line string regardless of calling context.

=head2 ps

  $ary_ref = $client->ps;
  $ary_ref = $client->ps(user => "jdoe", process => "gui_interface", ...);

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
match I<all> the corresponding values.

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
		  '/ccmdb/tbd/slc/db',
		  '/ccmdb/tbd/eai/db',
		  ...
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

=head2 status

  $ary_ref = $client->status;

Executes B<ccm status> and returns a reference to an array of references,
one per CM Synergy session. Each reference points to a hash
containing pairs of field names (e.g. C<database>) and values
for that particular session.

The available keys are a subset of the keys returned by the
L</ps> method: C<rfc_address>, C<database>, C<user>, and C<process>.

Note: Unlike the output of the B<ccm status> command, the value
for C<database> has a trailing C<"/db">. This makes it consistent
with the session attribute C<database> and the return value of L</ps>.

Here's an example of the value returned by C<status> 
as formatted by L<Data::Dumper>:

  $status = [
      {
	'process' => 'gui_interface',
	'database' => '/ccmdb/scm/support/db',
	'rfc_address' => 'tiv01:53020:160.50.76.15',
	'user' => 'rschupp'
      },
      {
	'process' => 'gui_interface',
	'database' => '/ccmdb/scm/support/db',
	'rfc_address' => 'wmuc111931:4661:160.50.136.201',
	'user' => 'rschupp'
      },
      {
	'process' => 'cmd_interface',
	'database' => '/ccmdb/test/tut51/db',
	'rfc_address' => 'tiv01:53341:160.50.76.15',
	'user' => 'rschupp'
      }
  ];

=head2 version

  $short_version = $client->version;
  ($full_version, $schema_version, 
    $informix_version, @patches) = $client->version;

Returns version info about the CM Synergy installation.
In a scalar context C<version> returns the (short) CM Synergy version number,
e.g. "6.2". In an array context the following information is returned:

=over 4

=item *

the full CM Synergy version (e.g. "6.2.3041")

=item *

the database schema version (e.g. "6.2")

=item *

the Informix version (e.g. "9.21.UC3X6")

=item *

a possible empty array of applied CM Synergy patches

=back

=head2 trace

  $client->trace($trace_level);
  $client->trace($trace_level, $trace_filename);

This method enables trace information to be written.

Trace levels C<$trace_level> are as follows:

=over 4

=item 0

trace disabled

=item 1

trace session start/stop; show parameters and exit code for all invocations of
CMSynergy CLI

=item 2

trace method autoloading; show queries synthesized from shortcuts

=item 8

show complete output for all invocations of CMSynergy CLI

=back

Initially trace output is written to C<STDERR>.  If C<$trace_filename> is
specified and can be opened in append mode then all trace
output is redirected to that file. 
A warning is generated if the file can't be opened.
Further calls to C<trace> without a C<$trace_filename> do not alter where
the trace output is sent. If C<$trace_filename> is C<undef>, then
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

  $client->trace_msg($message_text);
  $client->trace_msg($message_text, $min_level);

Writes C<$message_text> to the trace file if trace is enabled.
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

=head2 databases

  @databases = $client->databases;
  @databases = $client->databases($servername);

Returns an array containing the names of all known CM Synergy databases. 

Note: This method does not work on Windows.

=head2 hostname

  $hostname = $client->hostname.

The hostname as returned by B<ccm_hostname> (which might be different
from what L<POSIX/uname> returns).

=head1 SEE ALSO

L<VCS::CMSynergy>,
L<VCS::CMSynergy::Object> 

=head1 AUTHORS

Roderich Schupp, argumentum GmbH <schupp@argumentum.de>

=head1 COPYRIGHT AND LICENSE

The VCS::CMSynergy::Client module is Copyright (c) 2001-2005 argumentum GmbH, 
L<http://www.argumentum.de>.  All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=cut

