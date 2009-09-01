package VCS::CMSynergy::Client;

our $VERSION = do { (my $v = q$Revision$) =~ s/^.*:\s*//; $v };

=head1 NAME

VCS::CMSynergy::Client - base class for CM Synergy methods that don't require a session

=head1 SYNOPSIS

  use VCS::CMSynergy::Client;

  $client = VCS::CMSynergy::Client->new(%attr);

  $ary_ref = $client->ps;
  $short_version = $client->version;
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
use constant _pathsep => is_win32 ? "\\" : "/" ;

our ($Debug, $Debugfh, $Error, $Ccm_command, $Default);

use Exporter();
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
    is_win32 _pathsep $Debug $Error $Ccm_command
    _exitstatus _error _usage);
sub _usage(\@$$$);

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

our %opts = 
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
	croak(__PACKAGE__."::new: unrecognized argument: $arg") 
	    unless exists $opts{$arg};

	$self->{$arg} = $value;
    }

    return $self->set_error("CCM_HOME is not set (neither as parameter to new() nor in environment")
	unless defined $self->{CCM_HOME};
    $self->{env}->{CCM_HOME} = delete $self->{CCM_HOME};

    my $ccm_exe = File::Spec->catfile(
	$self->{env}->{CCM_HOME}, "bin", "ccm$Config{_exe}");
    return $self->set_error("CCM_HOME = `$self->{env}->{CCM_HOME}' does not point to a valid CM Synergy installation")
	unless -x $ccm_exe || ($^O eq 'cygwin' && -e $ccm_exe);
	# NOTE: -x $ccm_exe fails on cygwin
    $self->{ccm_exe} = $ccm_exe;

    return $self;
}


sub _memoize_method 
{
    _usage(@_, 3, 3, '$class, $method, $code');

    my ($class, $method, $code) = @_; 
    croak(__PACKAGE__.qq[::_memoize_method: "$code" must be a CODE ref]) 
	unless ref $code eq "CODE";
    my $slot = $method;

    no strict 'refs';
    no warnings 'redefine';
    *{"${class}::${method}"} = sub 
    {
	my $self = shift;
	$self->{$slot} = &$code($self, @_) unless exists $self->{$slot};
	return $self->{$slot};
    };
}


sub start
{
    my ($this, %args) = @_;
    $this = __PACKAGE__->_default unless ref $this;

    return VCS::CMSynergy->_start($this, %args);
}


sub _default	{ $Default ||= shift->new(); }


sub ccm						# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;

    my ($rc, $out, $err) = $this->_ccm(@_);

    return wantarray ? ($rc, $out, $err) : 1 if $rc == 0;
    return $this->set_error($err || $out, undef, 0, $rc, $out, $err);
    # NOTE: most failing ccm commands issue there error messages on stdout!
}


my $ccm_prompt = qr/^ccm> /m;		# NOTE the trailing blank

sub _ccm
{
    my $this = shift;
    my $opts = @_ && ref $_[-1] eq "HASH" ? pop : {};

    $Error = $this->{error} = undef;
    $Ccm_command = $this->{ccm_command} = join(" ", @_);

    my %ropts = %$opts;
    my $rin  = exists $ropts{in}  ? delete $ropts{in}  : \undef;
    my $rout = exists $ropts{out} ? delete $ropts{out} : do { my $s; \$s };
    my $rerr = exists $ropts{err} ? delete $ropts{err} : do { my $s; \$s };
    my $rc;

    my $t0;
    if ($Debug)
    {
	$t0 = [ Time::HiRes::gettimeofday() ];
	if ($Debug >= 8)
	{
	    # NOTE: log the command _before_ executing it to help
	    # diagnose "hung" scripts (e.g. a ccm command waiting for 
	    # user confirmation)
	    $this->trace_msg("<- ccm($this->{ccm_command})\n", 8);
	}
    }

    CCM:
    {
	if ($this->{coprocess})
	{
	    USE_COPROCESS:
	    {
		# don't use copress when using fancy run3() arguments
		last USE_COPROCESS if %$opts;

		# arguments cannot contain newlines in "interactive" ccm sessions
		last USE_COPROCESS if grep { /\n/ } @_;

		my ($dev, $ino) = stat(".") or last USE_COPROCESS;
		if ($this->{co_cwd_dev} != $dev || $this->{co_cwd_ino} != $ino)
		{
		    # working directory has changed since coprocess was spawned:
		    # shut down coprocess and start a new one
		    # NOTE: don't call _ccm here (infinite recursion)
		    $this->_kill_coprocess;
		    unless ($this->_spawn_coprocess)
		    {
			carp(__PACKAGE__ . " _ccm: can't re-establish coprocess (because cwd changed): $this->{error}\n" .
			     "-- ignoring UseCoprocess from now on");
			last USE_COPROCESS;
		    }
		    $Debug && $this->trace_msg(
			"spawned new coprocess because cwd changed (pid=".$this->{coprocess}->pid.")\n", 8);
		}

		# NOTE: "interactive" command arguments that contain blanks must 
		# be quoted with double quotes; AFAICT there is no way 
		# to quote embedded quotes!
		$this->{coprocess}->print(
		    join(" ", map { qq["$_"] } @_), "\n");

		$this->{coprocess}->expect(undef, -re => $ccm_prompt)
		    or return _error("expect error: ".$this->{coprocess}->error);

		# on Windows, treat output as if read in "text" mode
		$$rout = $this->{coprocess}->before;

		$this->{coprocess}->print("set error\n");
		$this->{coprocess}->expect(undef, -re => $ccm_prompt)
		    or return _error("expect error: ".$this->{coprocess}->error);
		my $set = $this->{coprocess}->before;
		($rc) = $set =~ /^(\d+)/
		    or return _error("unrecognized result from `set error': $set");
		($rc, $$rerr) = (_exitstatus($rc), "");
		last CCM;
	    }
	}

	# simple ccm sub process
	$rc = $this->run([ $this->ccm_exe, @_ ], $rin, $rout, $rerr, \%ropts);
    }

    unless (exists $opts->{out})
    {
	$$rout =~ s/\015\012/\012/g if is_win32;	# as if read in :crlf mode
	$$rout =~ s/\n\z//;				# chomp
    }
    unless (exists $opts->{err})
    {
	$$rerr =~ s/\015\012/\012/g if is_win32;	# as if read in :crlf mode
	$$rerr =~ s/\n\z//;				# chomp
    }

    if ($Debug)
    {
	my $elapsed = sprintf("%.2f", Time::HiRes::tv_interval($t0));
	if ($Debug >= 8)
	{
	    $this->trace_msg("-> rc = $rc [$elapsed sec]\n", 8);
	    $this->trace_msg("-> out = \"$$rout\"\n", 8) unless exists $opts->{out};
	    $this->trace_msg("-> err = \"$$rerr\"\n", 8) unless exists $opts->{err};
	}
	else
	{
	    my $success = $rc == 0 ? "ok" : "failed";
	    $this->trace_msg("ccm($this->{ccm_command}) = $success [$elapsed sec]\n");
	}
    }

    $this->{out} = $$rout unless exists $opts->{out};
    $this->{err} = $$rerr unless exists $opts->{err};

    return ($rc, exists $opts->{out} ? undef : $$rout, exists $opts->{err} ? undef : $$rerr);
}

sub run
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;

    # augment %ENV
    my $env = $this->{env};
    local @ENV{keys %$env} = values %$env if defined $env;

    # don't screw up global $? (e.g. when being called 
    # in VCS::CMSynergy::DESTROY at program termination)
    local $?;			
    run3(@_);
    return $?;
}

sub _spawn_coprocess
{
    my $self = shift;

    unless (eval "use Expect 1.15; 1")
    {
	$Error = $self->{error} = $@;
	return;
    }

    # augment %ENV
    my $env = $self->{env};
    local @ENV{keys %$env} = values %$env if defined $env;

    my $exp = Expect->new
	or $Error = $self->{error} = "Expect->new failed", return;
    ($exp->log_stdout(0) && $exp->slave->set_raw && $exp->set_raw)
	or $Error = $self->{error} = $exp->exp_error, return;
    $exp->spawn($self->ccm_exe)
	or $Error = $self->{error} = $exp->exp_error, return;
    
    # look for initial "ccm> " prompt
    $exp->expect(undef, -re => $ccm_prompt)
	or $Error = $self->{error} = $exp->exp_error, return;

    $self->{coprocess} = $exp;

    # remember coprocess' working directory
    # (so that we can detect whether the current working directory
    # of the main process has diverged)
    @$self{qw/co_cwd_dev co_cwd_ino/} = stat(".");

    return 1;
}

sub _kill_coprocess
{
    my $self = shift;
    $self->{coprocess}->print("exit\n");
    # FIXME: kill it just for paranoia (must save pid before line above!)
    $self->{coprocess} = undef;
}

# helper: inverse function of POSIX::WEXITSTATUS()
sub _exitstatus	{ return $_[0] << 8; }

# helper: return a triple ($rc, $out, $err)
sub _error	{ return (_exitstatus(255), "", $_[0]) }

# helper: check usage
# check min ($minargs) and max ($maxargs) number of arguments;
# use $maxargs=undef for unlimited arguments;
# for methods, you should shift $self off @_ before calling _usage();
# croak with message constructed from $usage
sub _usage(\@$$$)
{
    my ($args, $minargs, $maxargs, $usage) = @_;
    return 1 if $minargs <= @$args && (!defined $maxargs || @$args <= $maxargs);
    my $fullname = (caller(1))[3];
    (my $method = $fullname) =~ s/^.*:://;
    croak("$fullname: called with invalid number of arguments" .
          "\n  should be $usage");
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
    $this = __PACKAGE__->_default unless ref $this;
    return $this->{env}->{CCM_HOME};
}

sub ccm_exe					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;
    return $this->{ccm_exe};
}

sub out 					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;
    return wantarray ? split(/\n/, $this->{out}) : $this->{out};
}

sub err 					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;
    return $this->{err};
}

# NOTE: we can't memoize "version", as the memoizing wrapper
# assumes an object (not a class) as invocant
sub version					# class/instance method
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;

    my $version = $this->_version;
    return @$version{qw(cmsynergy schema informix patches)} if wantarray;
    return $version->{short};
}

__PACKAGE__->_memoize_method(_version => sub
{
    my $self = shift;

    # "version" is not a recognized "interactive" command
    local $self->{coprocess} = undef;

    my ($rc, $out, $err) = $self->_ccm(qw/version -all/);
    return $self->set_error($err || $out) unless $rc == 0;

    my %version;
    my $cmsynergy_rx = qr{(?:CM Synergy|SYNERGY/CM|Telelogic Synergy|IBM Rational Synergy)};
    ($version{cmsynergy}) = $out =~ /^$cmsynergy_rx Version\s+(\S*)$/imo
	or return $self->set_error("can't recognize version from `$out'");
    ($version{short}) = $version{cmsynergy} =~ /^(\d+\.\d+)/;
    
    ($version{schema}) = $out =~ /^$cmsynergy_rx Schema Version\s+(.*)$/imo;
    ($version{informix}) = $out =~ /^Informix.* Version\s+(.*)$/imo;
    $version{patches} = [ split(/\n/, $1) ]
	if $out =~ /^$cmsynergy_rx Patch Version\s+(.*?)(?:\Z|^$cmsynergy_rx|^Informix)/imso; 
    return \%version;
});


sub ps	
{
    my ($this, @filter) = @_;
    $this = __PACKAGE__->_default unless ref $this;

    # "ps" is not a recognized "interactive" command
    local $this->{coprocess} = undef;

    my ($rc, $out, $err) = $this->_ccm(ps => @filter);
    return $this->set_error($err || $out) unless $rc == 0;

    # split at "rfc address..." header lines;
    # discard first item (the line "All Rfc processes..." or
    # "Processes with...")
    # NOTE: We do it this way (and not by splitting into lines
    # first, then recognizing "rfc address..." lines as 
    # record headers) to work around a Synergy glitch:
    # if the single line in $CCM_HOME/etc/.router.adr ends with
    # a newline, then the record for the router process
    # will look like:
    #   rfc address (macarthur:5418:127.0.1.1:192.168.57.10
    #   )
    #         process (router)
    #         user (ccm_root)
    # i.e. the address contains an embedded newline. This
    # breaks line-based parsing.
    my @rfcs = split(/^rfc address \((.*?)\)/sm, $out);
    shift @rfcs;	

    my @ps;
    while (@rfcs)
    {
	my ($rfc_address, $rest) = splice @rfcs, 0, 2;
	chomp ($rfc_address, $rest);

	my %fields = $rest =~ /^\t(\S+) \((.*?)\)/gm;

	# the ps entry for the objreg process contains lines of the form
	#   db:/var/lib/telelogic/ccm65/tutorial_pre64sp1/db ()
	# transform the (database) paths into a list 
	# as the value of key "db"
	my @dbs;
	foreach my $key (keys %fields)
	{
	    if ($key =~ /^db:(.*)$/)
	    {
		push @dbs, $1;
		delete $fields{$key};
	    }
	}

	$fields{rfc_address} = $rfc_address;
	$fields{db} = \@dbs if @dbs;

	push @ps, \%fields;
    }

    return \@ps;
}


sub status	
{
    my $this = shift;
    $this = __PACKAGE__->_default unless ref $this;

    my ($rc, $out, $err) = $this->_ccm(qw/status/);
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


# FIXME does not work on Windows 
# (in fact, it only works on the host where Synergy's Informix engine is running)
sub databases	
{
    my ($this, $servername) = @_;
    $this = __PACKAGE__->_default unless ref $this;

    my @server_status = 
	(File::Spec->catfile($this->ccm_home, qw/bin ccmsrv/), qw/status/);
    push @server_status, -s => $servername if defined $servername;

    my ($out, $err);
    my $rc = $this->run(\@server_status, \undef, \$out, \$err);
    chomp ($out, $err);
    return $this->set_error($err || $out) unless $rc == 0;

    # strip leading/trailing stuff
    my ($list) = $out =~ /^===.*?$(.*?)^There is a total/ms;
    return $this->set_error(qq[unrecognized output from "@server_status": $out])
	unless defined $list;
    return grep { !/dbpath not available/ }
           map  { (split(' ', $_, 3))[2]  } 
	   split(/\n/, $list);
}

# FIXME does not work on windows 
sub hostname
{
    my ($this, @filter) = @_;
    $this = __PACKAGE__->_default unless ref $this;

    our %Hostname;				# cache by CCM_HOME
    my $ccm_home = $this->ccm_home;
    unless (exists $Hostname{$ccm_home})
    {
	my ($out, $err);
	my $rc = $this->run([ File::Spec->catfile($ccm_home, qw/bin util ccm_hostname/) ], \undef, \$out, \$err);
	chomp($out, $err);
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

    $Error = $this->{error} = $error;

    # try the HandleError routine if one was provided;
    # consider the error handled if it returns true
    my $handler = $this->{HandleError};
    return wantarray ? @rv : $rv if $handler and &$handler($error, $this, $rv, @rv);

    # unless $method was explicitly specified, use our caller 
    # except skip private methods of VCS::CMsynergy... packages
    unless (defined $method)
    {
	$method = (caller(0))[3];
	for (my $n = 1;; $n++)
	{
	    my $next = (caller($n))[3];
	    last unless defined $next;
	    $method = $next;
	    last unless $method =~ /^VCS::CMSynergy.*::_\w*$/;
	}
    }

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

=head2 ccm_exe

Returns the absolute pathname of the B<ccm> executable.

=head2 trace

  $client->trace($trace_level);
  $client->trace($trace_level, $trace_filename);

This method enables trace information to be written.

Trace levels C<$trace_level> are as follows:

=over 4

=item 0

trace disabled

=item 1

trace session start/stop; show arguments, exit code and elapsed time
for all invocations of CMSynergy CLI

=item 5

trace method autoloading; show query shortcut processing

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

=head2 run

  $client->run(\@cmd, $in, $out, $err);

Runs C<run3> from L<IPC::Run3> with the given arguments in an
environment (C<$ENV{CCM_HOME}>, C<$ENV{PATH> etc) set up for C<$client>.
Returns the exit status (i.e. C<$?>) from executing C<@cmd>.

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

The VCS::CMSynergy::Client module is Copyright (c) 2001-2009 argumentum GmbH, 
L<http://www.argumentum.de>.  All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=cut

