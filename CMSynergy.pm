package VCS::CMSynergy;

our $VERSION = sprintf("%d.%02d", q%version: 1.06 % =~ /(\d+)\.(\d+)/);

=head1 NAME

VCS::CMSynergy - Perl interface to Telelogic CM Synergy

=head1 SYNOPSIS

  use VCS::CMSynergy;

  $ccm = VCS::CMSynergy->new(%attr);

  ($rc, $out, $err) = $ccm->ccm($ccm_command, @ccm_args);
  ($rc, $out, $err) = $ccm->any_ccm_command(@ccm_args);

  $ary_ref = $ccm->query(@ccm_args);
  $ary_ref = $ccm->query_object($query);
  $ary_ref = $ccm->query_arrayref($query, @keywords);
  $ary_ref = $ccm->query_hashref($query, @keywords);

  $ary_ref = $ccm->finduse(@args);
  $path = $ccm->findpath($file_spec, $proj_vers);

  $ary_ref = $ccm->history(@ccm_args);
  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

  $ary_ref = $ccm->ls(@ccm_args);
  $ary_ref = $ccm->ls_object($file_spec);
  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);
  $ary_ref = $ccm->ls_hashref($file_spec, @keywords);

  $value = $ccm->attribute($attr_name, $file_spec);
  $old_value = $ccm->attribute($attr_name, $file_spec, $new_value);
  $hash_ref = $ccm->attributes($file_spec);

  $delim = $ccm->delimiter;
  @types = $ccm->types;

  ($ccm, $schema, $informix, @patches) = $h->version;
  @ary = $h->databases;
  $ary_ref = $h->ps;
  $ary_ref = $h->ps(\%attr);
  $ary_ref = $h->status;

This synopsis above only lists the major methods.
Methods for administering users and their roles are
described in the L<VCS::CMSynergy::Users> documentation.

=head1 DESCRIPTION

  use VCS::CMSynergy;

  my $ccm = VCS::CMSynergy->new(database => "/ccmdb/test/tut62/db");

  $ccm->checkout(qw(foo/bar.c@foo~user -to test))
    or die "checkout failed: $ccm->{error}";

  my $csrcs = $ccm->query_hashref("type = 'csrc'",
				  qw(displayname modify_time));
  if ($csrcs)
  {
    print "$_->{displayname} $->{modify_time}\n" foreach (@$csrcs);
  }

=head1 METHODS

=over 4

=cut

use v5.6.0;				# FIXME: have only tested with v5.6.1 
use strict;
use Carp;

use Config;
use File::Spec;
use IPC::Open3;
use POSIX qw(_exit);

# Unix only
use IO::Handle qw(_IOLBF);
use IO::Select;
use IO::File;
use IO::Pipe;				# make ActiveState PerlApp happy

use File::Temp qw(tempfile);		# in Perl core v5.6.1 and later


our $ccm_prompt = qr/^ccm> /;		# NOTE the trailing blank
our $use_ccm_coprocess = 0;
our ($debug, $debugfh, $error, $AUTOLOAD);

{
    $debug = $ENV{CMSYNERGY_TRACE} || 0;
    $debugfh = \*STDERR;
    if ($debug)
    {
	if ($debug =~ /^\d+$/) 			# CMSYNERGY_TRACE="digits"
	{ 
	    # level=digits, tracefile=stderr
	    VCS::CMSynergy->trace($debug, undef); 	
	}
	elsif ($debug =~ /^(\d+)=(.*)/) 	# CMSYNERGY_TRACE="digits=filename"
	{
	    # level=digits, tracefile=filename
	    VCS::CMSynergy->trace($1, $2); 
	}
	else					# CMSYNERGY_TRACE="filename"
	{
	    # level=2, tracefile=filename
	    VCS::CMSynergy->trace(2, $debug); 	
	}
    }

    # Use Memoize.pm if it's available, otherwise define
    # sub VCS::CMSynergy::memoize as a no-op.
    eval { require Memoize; import Memoize; };
    *memoize = sub { 1; } if $@;
}

=item C<new>

  my $ccm = VCS::CMSynergy->new( database => "/ccmdb/foo/db" )
              or die $VCS::CMSynergy::error;

Starts a new CM Synergy session. Returns a session handle if it succeeds. 

If fails to start a session, it returns C<undef> and sets 
C<$VCS::CMSynergy::error> to the error string printed by CM Synergy.

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

=item C<database>

CM Synergy database path. 

This is the only attribute required on Unix systems.

=item C<host>

CM Synergy engine host to use.

It defaults to the local host.

=item C<role>

User's initial CM Synergy role.

It defaults to C<developer>.

=item C<user>

CM Synergy user. 

This attribute is available and required on Windows systems only.

=item C<password>

User's password. 

This attribute is available and required on Windows systems only.

=item C<PrintError>

This attribute can be used to force errors to generate warnings (using
L<Carp/carp>) in addition to returning error codes in the normal way.  
When set to true, any method which results in an error occuring will cause
the corresponding C<$ccm-E<gt>{error}> to be printed to stderr.

It defaults to 1.

L</PrintError> and L</RaiseError> below are stolen from the excellent
L<DBI> module.

=item C<RaiseError>

This attribute can be used to force errors to raise exceptions 
(using L<Carp/croak>) rather than simply return error codes in the normal way. 
When set to true, any method which results in an error will cause
the DBI to effectively do a C<die> with the actual C<$ccm->E<gt>C<{error}>
as the message. 

It defaults to 0.

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

=item C<ini_file>

CM Synergy ini file to use. 

In contrast to the CM Synergy C<ccm start> command there is I<no>
default ini file whatsoever consulted. (On Unix systems this is achieved
by executing C<ccm start> with the option C<-f /dev/null>.) The reason
is that we want scripts to behave in a reproducible way. Otherwise
the script might accidentally work with the current contents of
the current user's ini file, but might fail when invoked by another user.
Or it might fail when invoked by the same user at a later time because of
changes to her ini file (e.g. because of another session between
invocations of the script). So if you really want to rely on an ini file,
you have to supply it explicitly.

=item C<CCM_ADDR>

Specifies the RFC address of an established CM Synergy session.

If you specify this attribut L</new> does not create a new session,
but will use the one specified. Also, destruction of the returned session
handle will not cause a C<ccm stop>.

Note that there is no default value. In particular, L</new> ignores
the environment variable of the same name.

=item C<CCM_HOME>

Value of the C<CCM_HOME> environment variable to use for this session.

It defaults from the environment variable of the same name,
i.e. C<$ENV{CCM_HOME}>.

This is only of interest if you have multiple version of CM Synergy
installed. You can have simultaneous sessions using different
CM Synergy versions (the module takes care of setting the C<CCM_HOME>
variable appropriately before issuing any C<ccm> commands). 


FIXME:
describe sessions attributes (RaiseError,PrintError,CCM_HOME,...)

=back

=cut

sub new
{
    my $class = shift;
    my %args = @_;
    # FIXME: check %args for legal keys, currently
    #	CCM_HOME CCM_ADDR database host ini_file role PrintError RaiseError
    #   user password (Win32 only, FIXME)
    #   required: database (Win32: user, password, host) except CCM_ADDR is given

    my $self = 
    {
	CCM_HOME	=> $ENV{CCM_HOME},
	PrintError	=> 1,
	RaiseError	=> 0,
	HandleError	=> undef,
	IgnoreError	=> undef,
	error		=> undef,
    };	
    foreach (qw(CCM_HOME 
    	        database host role 
		user password 
		PrintError RaiseError HandleError
		ini_file CCM_ADDR))
    {
	$self->{$_} = $args{$_} if exists $args{$_};
    }

    if ($self->{CCM_ADDR})
    {
	$self->{reuse} = 1;
	$class->trace_msg("reusing session `$self->{CCM_ADDR}'\n") if $debug;
    }
    else
    {
	unless (defined $self->{ini_file})
	{
	    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
	    {
		# NOTES: 
		# (1) "ccm start -f nul ..." doesn't work on Windows
		# (leads to ccm_seng error), so use an empty ini_file instead
		# (2) we can't use UNLINK=>1 with tempfile, because 
		# the actual unlink may occur before the session is
		# stopped and Windows refuses removing the "busy" file
		(undef, $self->{ini_file}) = 
		    tempfile("ccmXXXX", SUFFIX => ".ini", UNLINK => 0);
		$self->{unlink_ini_file} = 1;
	    }
	    else
	    {
		$self->{ini_file} = File::Spec->devnull;
	    }
	}

	my @start = qw(start -m -q -nogui);
	push @start, "-d", $self->{database}	if defined  $self->{database};
	push @start, "-h", $self->{host}	if defined  $self->{host};
	push @start, "-f", $self->{ini_file}	if defined  $self->{ini_file};
	push @start, "-r", $self->{role}	if defined  $self->{role};
	if ($^O eq 'MSWin32' || $^O eq 'cygwin')
	{
	    push @start, "-n", $self->{user}	if defined  $self->{user};
	    push @start, "-pw", $self->{password} if defined  $self->{password};
	    # FIXME: CCM 6.2 with ESD uses -pw on UNix, too
	}

	my ($rc, $out, $err) = 
	    _ccmexec($self->{CCM_HOME}, _ccm_exe($self->{CCM_HOME}), @start);
	if ($rc != 0)
	{
	    # set_error does not work as a class method, 
	    # hence we must handle *Error by hand

	    $VCS::CMSynergy::error = $err || $out;
	    
	    my $handler = $self->{HandleError};
	    return undef if $handler and &$handler($VCS::CMSynergy::error, $self);

	    my $msg = __PACKAGE__ . " new: $VCS::CMSynergy::error";
	    croak($msg)	if $self->{RaiseError};	# die if RaiseError is set
	    carp($msg)	if $self->{PrintError};	# warn if PrintError is set

	    return undef;
	}

	$self->{CCM_ADDR} = $out;
	$class->trace_msg("started session `$self->{CCM_ADDR}'\n") if $debug;
    }

    bless $self, $class;

    if ($use_ccm_coprocess)
    {
	my $exp_err;
	EXP:
	{
	    local $ENV{CCM_HOME} = $self->{CCM_HOME};
	    local $ENV{CCM_ADDR} = $self->{CCM_ADDR};
	    my $exp = new Expect
		or $exp_err = "Expect: new failed", last EXP;
	    ($exp->log_stdout(0) && $exp->slave->stty(qw(-echo raw)))
		or $exp_err = $exp->exp_error, last EXP;
	    $exp->spawn(_ccm_exe($self->{CCM_HOME}))
		or $exp_err = $exp->exp_error, last EXP;
	    
	    # look for initial "ccm> " prompt
	    $exp->expect(undef, '-re', $ccm_prompt)
		or $exp_err = $exp->exp_error, last EXP;
	    $self->{exp} = $exp;
	}
	carp(__PACKAGE__ . " new: can't establish ccm coprocess: $exp_err - falling back to single shot ccm processes")
	    unless $self->{exp};
	# FIXME better fall back description
    }

    # cache some info from database;
    # this also doubles as a test for a valid session
    my ($rc, $out, $err);
    {
	local $self->{IgnoreError} = 0;
        ($rc, $out, $err) = $self->ccm(qw(delimiter));
    }
    return $self->set_error($err || $out) unless $rc == 0;
    $self->{delimiter} = $out;
    $self->{objectname_re} = qr/^(.*?)\Q$self->{delimiter}\E(.*?):(.*?):(.*?)$/;
	
    # sanitize database path
    # FIXME: how do I determine database if $self->{reuse}? from $ccm->status?
    # FIXME: parsing is broken in case of an NT server
    if (defined $self->{database})
    {
	$self->{database} =~ s{/+}{/}g;
	$self->{database} =~ s{/$}{};
	$self->{database} =~ s{(/db)?$}{/db};
    }

    if ($debug)
    {
	use Data::Dumper;
	$self->trace_msg(Data::Dumper->Dump([$self], ["$self"]), 9);
    }

    return $self;
}

=item C<DESTROY>

  $ccm->DESTROY;

Stops the CM Synergy session represented by the session handle
by executing C<ccm stop> (unless the session has the C<reuse>
attribut set).

There is usually no need to call this method explicitly, as it
is invoked by the Perl runtime when the Perl process exits
(either by calling C<exit> or because of a C<die>).
Hence, a script using the L<VCS::CMSynergy> module will not leave
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

=cut

sub DESTROY 
{
    my $self = shift;

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
    
    local $self->{IgnoreError} = 1;

    if ($self->{reuse})
    {
	$self->ccm(qw(exit)) if $self->{exp};	# shutdown coprocess
    }
    elsif ($self->ccm(qw(stop)))
    {
	$self->trace_msg("stopped session $self->{CCM_ADDR}\n") if $debug;
    }
    unlink($self->{ini_file}) if $self->{unlink_ini_file};
}

=item C<ccm>

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
      or die "checkout failed: $ccm->{error}";

Note that you must pass every C<ccm> argument or option as a single Perl 
argument. For literal arguments the C<qw()> notation may come in handy, e.g.

  ($rc, $out, $err) = $ccm->ccm(qw(finduse -state working));

Most specialized methods in the VCS::CMSynergy module are ultimately implemented
via the L</ccm> method. Using it directly is only recommended for
commands that perform some action, e.g. B<ccm checkout>, as opposed to 
query-like commands. For the latter, e.g. B<ccm query>, use one of the 
methods that return the information in structured form, 
e.g. L</query_arrayref> or L</query_hashref>, instead of having 
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

=cut

sub ccm					# class/instance method
{
    my ($this, $cmd, @args) = @_;
    ref $this ? $this->{error} : $VCS::CMSynergy::error = undef;

    my ($rc, $out, $err);

    # NOTE: Certain comands (e.g. "ps") are not accepted from the "ccm>" prompt.
    if (ref $this && $this->{exp} && $cmd !~ /^(ps|version)$/)
    {
	my $exp =  $this->{exp};
	EXP: 
	{
	    my ($match, $set);

	    # NOTE: "ccm>" command arguments that contain blanks must 
	    # be quoted with double quotes. AFAICT there is no way 
	    # to quote embedded quotes!
	    $exp->print(join(" ", $cmd, map { "\"$_\"" } @args), "\n");
	    ($match, $err, undef, $out, undef) =
		$exp->expect(undef, '-re', $ccm_prompt);
	    ($rc, $out, $err) = (_exitstatus(255), "", "expect error: $err"), last EXP 
		unless $match;

	    chomp($out);

	    $exp->print("set error\n");
	    ($match, $err, undef, $set, undef) =
		$exp->expect(undef, '-re', $ccm_prompt);
	    ($rc, $out, $err) = (_exitstatus(255), "", "expect error: $err"), last EXP 
		unless $match;
	    ($rc, $out, $err) = (_exitstatus(255), "", "unrecognized result from `set error': $set"), last EXP
		unless ($rc) = $set =~ /^(\d+)/;
	    ($rc, $err) = (_exitstatus($rc), "");
	}
    }
    else
    {
	local $ENV{CCM_ADDR} = $this->{CCM_ADDR} if ref $this;
	($rc, $out, $err) = 
	    _ccmexec($this->_ccm_home, _ccm_exe($this->_ccm_home), $cmd, @args);
    }
    $this->trace_msg("<- ccm($cmd @args)= " .
                     ($debug >= 8 ? "($rc, '$out', '$err')" : $rc==0) .
		     "\n") if $debug;

    return wantarray ? ($rc, $out, $err) : 1 
	if $rc == 0 || (ref $this && $this->{IgnoreError});
    return $this->set_error($err || $out, undef, 0, $rc, $out, $err);
    # NOTE: most failing ccm commands issue there error messages on stdout!
}

# helper: return CCM_HOME in class/instance context
sub _ccm_home					# class/instance method
{
    my $this = shift;
    return ref $this ? $this->{CCM_HOME} : $ENV{CCM_HOME};
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
# execute a program with CCM_HOME set up appropriately
sub _ccmexec
{
    my ($ccm_home, $prog, @args) = @_;
    my ($rc, $out, $err) = (undef, "", "");

    return (_exitstatus(255), "", "CCM_HOME not set") unless defined $ccm_home;

    local (*NULL);			
    open(NULL, File::Spec->devnull) or die "can't open /dev/null: $!";
    # NOTE: NULL will be closed (in parent) by open3

    # NOTE: On operating systems with a broken "fflush(NULL)"
    # (e.g. Solaris), Perl does _not_ flush all open file handles
    # before a fork() (called by open3() below). Hence the user
    # might see "double output". The workaround below does not
    # completely solve the problem, but at least we can explicitly
    # flush all file handles we know about (STDOUT, STDERR and $debugfh).
    unless ($Config{fflushNULL})
    {
	STDOUT->flush;
	STDERR->flush;
	$debugfh->flush if defined $debugfh;
    }

    my ($outfh, $errfh, $pid);
    if ($^O eq 'MSWin32')
    {
	# NOTE: On Win32, `exec LIST´ (as called by open3) will mung
	# the LIST elements, e.g. an element with embedded blanks
	# will result in two or more arguments passed to the
	# exec'ed program, an embedded '>' will result in IO
	# redirection. This is a bug and may be fixed in Perl
	# versions later than 5.6.1 (cf. Changelog entries
	# #12563 and #12559). The workaround below
	# fixes blanks and IO redirectors, but doesn't help
	# for embedded double quotes and substrings like "%path%"
	# where the Windows shell does variable substitution even
	# when inside double quotes. FIXME
	# FIXME: This doesn't seem to be true any more, at least not for
	# ActivePerl build 631 running on Windows 2000.
	@args = map { /[\s<|>]/ ? "\"$_\"" : $_ } @args;
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
    # FIXME: NOTE on SIGCHLD probs
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
	local $ENV{CCM_HOME} = $ccm_home;

	$pid = open3("<&NULL", $outfh, $errfh, $prog, @args);
    };
    return (_exitstatus(255), "", $@) if $@;

    if ($^O eq 'MSWin32')
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
		    # if we close $fh before removal it's fileno is gone;
		    # hence removal is a noop; this leaves an invalid 
		    # file descriptor in the select set which causes the whole
		    # can_read loop to exit prematurely
		    $sel->remove($fh);	
		    $fh->close;
		}
	    }
	}
    }
    return (_exitstatus(255), "", "waitpid returned unexpected value")
	if waitpid($pid, 0) != $pid;
    $rc = $?;

    # on Windows, treat output as if read in "text" mode
    $out =~ s/\015\012/\012/g if $^O eq 'MSWin32' || $^O eq 'cygwin';

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

# helper: inverse function of POSIX::WEXITSTATUS
sub _exitstatus
{
    return $_[0] << 8;
}

=item C<delimiter>

  $delim = $ccm->delimiter;

Returns the database delimiter.

=cut

sub delimiter	
{ 
    return shift->{delimiter}; 
}

=item C<query>

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
result set of the query, you should look at the L</query_arrayref>
and L</query_hashref> methods that return this information in 
structured form. If you are only interested in the identity of
objects in the result set, you should look at the L</query_object> method.

Note that L</query> will probably produce
unpredictable results when the C<-format> option references attributes
that can have multi-line values, e.g. C<status_log>. 
L</query_arrayref> and L</query_hashref> handle this case correctly.

=cut

sub query
{
    my $self = shift;
    my ($rc, $out, $err);
    {
	# NOTE: if there are no hits, `ccm query' exits with status 1, 
	# but produces no output on either stdout and stderr
	local $self->{IgnoreError} = 1;
	($rc, $out, $err) = $self->ccm(qw(query -u), @_);
    }
    return [ split(/\n/, $out) ] if $rc == 0;
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out);
}

# helper: query with correct handling of multi-line attributes
sub _query
{
    my ($self, $query, @keywords) = @_;

    my (@wanted, @kw_object, @kw_finduse);
    for (my $i = 0; $i < @keywords; $i++)
    {
	if ($keywords[$i] eq 'object')
	{
	    push @wanted, "%objectname";
	    push @kw_object, $i;
	    next;
	}
	if ($keywords[$i] eq 'finduse')
	{
	    push @wanted, "?";			# doesn't matter
	    push @kw_finduse, $i;
	    next;
	}
	push @wanted, "%$keywords[$i]";
    }

    my $format = "\cD" . join("\cG", @wanted) . "\cG";

    my ($rc, $out, $err);
    {
	# NOTE: if there are no hits, `ccm query' exits with status 1, 
	# but produces no output on either stdout and stderr
	local $self->{IgnoreError} = 1;

	if (@kw_finduse)
	{
	    ($rc, $out, $err) = $self->_with_local_option(
		Object_format => $format,
		qw(ccm finduse -query), $query);
	}
	else
	{
	    ($rc, $out, $err) = $self->ccm(qw(query -u -ns -nf -format), $format, $query);
	}
    }
    return [ ] if $rc == _exitstatus(1) and $out eq "" and $err eq "";
    return $self->set_error($err || $out) unless $rc == 0;;

    my @result;
    $out =~ s/\A\cD//;				# trim leading record separator
    foreach my $row (split(/\cD/, $out))	# split into records 
    {
	# split into columns and translate "<void>" to undef
	my @cols = map { $_ eq "<void>" ? undef : $_ } split(/\cG/, $row);

	# make sure we have exactly @keywords+1 columns (because split may 
	# have discarded trailing empty fields, esp. for the last row)
	$#cols = @keywords;

	if (@kw_finduse)
	{
	    # parse finduse list (the last columns)
	    local $_ = pop @cols;
	    my @finduse = ();

	    unless (/Object is not used in scope/)
	    {
		s/\n\z//;
		s/\A\n\t//;
		@finduse = split(/\n\t/);
	    }

	    # fill in 'finduse' columns
	    $cols[$_] = \@finduse foreach (@kw_finduse);
	}
	else
	{
	    pop @cols;				# discard superficial last column
	}

	# objectify 'object' columns
	$cols[$_] = $self->object($cols[$_]) foreach (@kw_object);

	push @result, \@cols;
    }
    return \@result;
}

=item C<query_object>

  $ary_ref = $ccm->query_object($query);

Executes B<ccm query> with the query expression C<$query> 
and returns a reference to an array of C<VCS::CMSynergy::Object>s 
that satisfy the query.

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

Note that this query method does I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
B<-o owner>, as these can all be expressed by suitable sub clauses
of the C<$query> expression.

=cut

sub query_object
{
    my ($self, $query) = @_;
    return $self->set_error("no query string supplied") unless defined $query;

    my $ary_ref =  $self->_query($query, qw(object));
    return undef unless $ary_ref;

    # slice out the first column
    return [ map { $_->[0] } @$ary_ref ];
}

=item C<query_arrayref>

  $ary_ref = $ccm->query_arrayref($query, @keywords);

Executes B<ccm query> with the query expression C<$query> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. Returns a reference to an array of references,
one per result row. Each reference points to an array containing
the values of the keywords for that particular object in the result set
(in the order given by C<@keywords>). 

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array element is C<undef> (whereas B<ccm query> would print it as
the string "C<void>").

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing the object in the result set.

=item C<finduse>

The value is a reference to an array of names in project reference form
identifying in what parts of what projects the object is used.
This is the same list as reported by B<ccm finduse>. In fact, if this
keyword is given, L</query_arrayref> invokes B<ccm finduse -query $query>
rather than B<ccm query $query>.

=back 

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->query_arrayref("name match '*.c'", 
                                    qw(displayname type modify_time);
  foreach my $row (@$result)
  {
    my ($displayname, $type, $modify_time) = @$row;
    print "$displayname ($type) last modified at $modify_time\n";
    ...
  }

Note that this query method does I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
B<-o owner>, as these can all be expressed by suitable sub clauses
of the C<$query> expression.

=cut

sub query_arrayref
{
    my ($self, $query, @keywords) = @_;
    return $self->set_error("no query string supplied") unless defined $query;
    return $self->set_error("no keywords supplied") unless @keywords;

    return $self->_query($query, @keywords);
}

=item C<query_hashref>

  $ary_ref = $ccm->query_hashref($query, @keywords);

Executes B<ccm query> with the query expression C<$query> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. Returns a reference to an array of references,
one per result row. Each reference points to hash containing
attribute and value pairs where the keys are C<@keywords>.

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
hash element is C<undef> (whereas B<ccm query> would print it as
the string "C<void>").

The following names may also be used as keywords though they
are neither built-in nor attributes:

=over 4

=item C<object>

The value is a C<VCS::CMSynergy::Object> representing the object in the result set.

=item C<finduse>

The value is a reference to an array of names in project reference form
identifying in what parts of what projects the object is used.
This is the same list as reported by B<ccm finduse>. In fact, if this
keyword is given, L</query_arrayref> invokes B<ccm finduse -query $query>
rather than B<ccm query $query>.

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

Note that this query method does I<not> support any of the
shortcut query options of the B<ccm query> command, e.g.
"-o owner", as these can all be expressed by suitable sub clauses
of the C<$query> expression.

=cut

sub query_hashref
{
    my ($self, $query, @keywords) = @_;
    return $self->set_error("no query string supplied") unless defined $query;
    return $self->set_error("no keywords supplied") unless @keywords;

    my $ary_ref = $self->_query($query, @keywords);
    return undef unless $ary_ref;

    # convert each row (an array ref) into a hash ref (with @keywords as the keys)
    return [ map { my %h; @h{@keywords} = @$_; \%h } @$ary_ref ];
}

=item C<hierarchy_project_members>

FIXME: interface may change

  $ary_ref = $ccm->hierarchy_project_members(
    $proj_vers, $order_spec, @keywords);

Convenience function. Uses the built-in VCS::CMSynergy query function of the
same name with arguments ($proj_vers, $order_spec).
Uses L</query_hashref>, i.e. returns ref to array of hash refs. 
@keywords are passed to query_hashref.

NOTE: The VCS::CMSynergy function hierarchy_project_members accepts
only objectname as its first argument , but this method
accepts a VCS::CMSynergy::Object, an objectname, or a proj_vers.
Example:

  $sub_projs = $ccm->hierarchy_project_members(
    "guilib-darcy", "depth", qw(name wa_path status));
  print "$_->{name} $_->{status} $_->{wa_path}\n" foreach (@$sub_projs);

=cut

sub hierarchy_project_members
{
    my ($self, $proj_vers, $order_spec, @keywords) = @_;

    $proj_vers .= ":project:1" 
	unless ref $proj_vers || $proj_vers =~ /:project:1$/;

    return $self->query_hashref(
	"hierarchy_project_members('$proj_vers', '$order_spec')", @keywords);
}


=item C<history>

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
you should look at the L</history_arrayref>
and L</history_hashref> methods that return this information in 
structured form. 

=cut

sub history
{
    my ($self, @args) = @_;

    my ($rc, $out, $err) = $self->ccm(qw(history), @args);
    return undef unless $rc == 0;

    return [ split(/^\*+\n?/m, $out) ];
}

=item C<history_arrayref>

  $ary_ref = $ccm->history_arrayref($file_spec, @keywords);

Executes B<ccm history> for C<$file_spec> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. Returns a reference to an array of references,
one per history entry. Each reference points to an array containing
the values of the keywords for that particular object in the history
(in the order given by C<@keywords>). 

If there a no hits, a reference to an empty array is returned.  

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
array element is C<undef> (whereas B<ccm history> would print it as
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

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->history_arrayref('math.h-1:incl:1', 
                                      qw(displayname modify_time successors);
  foreach my $row (@$result)
  {
    my ($displayname, $modify_time, $successors) = @$row;
    print "$displayname: last modified at $modify_time\n";
    print "\t$_\n" foreach (@$successors);
    ...
  }

=cut

sub history_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    my $ary_ref = $self->history_hashref($file_spec, @keywords);
    return unless $ary_ref;
 
    return [ map { [ @$_{@keywords} ] } @$ary_ref ];
}


=item C<history_hashref>

  $ary_ref = $ccm->history_hashref($file_spec, @keywords);

Executes B<ccm history> for C<$file_spec> asking
for the values of the built-in keywords or attributes supplied
in C<@keywords>. Returns a reference to an array of references,
one per history entry. Each reference points to a hash containing
attribute and value pairs where the keys are C<@keywords>.

If there was an error, C<undef> is returned.

If the value of a keyword or an attribute is undefined or
the attribute is not present, the actual value of the corresponding
hash element is C<undef> (whereas B<ccm history> would print it as
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

Note that the keyword or attribute names given in C<@keywords> should I<not>
contain a leading C<%>. Example:

  my $result = $ccm->history_hashref('math.h-1:incl:1', 
                                     qw(displayname modify_time successors);
  foreach my $row (@$result)
  {
    print "$row->{displayname}: last modified at $row->{modify_time}\n";
    print "\t$_\n" foreach (@{ $row->{successors} });
    ...
  }

=cut

sub history_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    return $self->set_error("no file_spec supplied") unless defined $file_spec;
    return $self->set_error("no keywords supplied") unless @keywords;

    # turn special keyword `object' into `objectname'
    my %want;
    $want{$_} = $_ eq 'object' ? 'objectname' : $_ foreach(@keywords);

    my $format = join("\a", map { '%'.$want{$_} } @keywords);
    my $chunks = $self->history('-f', $format, $file_spec);
    return undef unless $chunks;

    my @history;
    foreach (@$chunks)
    {
	my ($description, $predecessors, $successors) =
	    /\A(.*)\n
	     ^Predecessors:(.*)
	     ^Successors:(.*)
	    /msx;
	my %hash;
	@hash{@keywords} = map { $_ eq '<void>' ? undef : $_ } 
			       split(/\a/, $description);

	# handle special keywords
	$hash{object} = $self->object($hash{object})
	    if $want{object};
	$hash{predecessors} =
	    [ map { $self->object($_) } split(' ', $predecessors) ] 
	    if $want{predecessors};
	$hash{successors} =
	    [ map { $self->object($_) } split(' ', $successors) ] 
	    if $want{successors};
	
	push @history, \%hash;
    }
    return \@history;
}

=item C<finduse>

  $ary_ref = $ccm->finduse(@args);

Executes the B<ccm finduse> command with the given C<@args> as parameters.
It returns a reference to an array of rows, usually one per C<file_spec> given
in C<@args>, or one per query result if C<-query $query_expression>
is present in C<@args>. Each row is a reference to an array, the first 
element is the description of the object followed by uses of the object
given in project reference form. If there are no uses of the object in the
given scope the array consists only of the first element.

If there was an error, C<undef> is returned.

Note that you must pass every B<ccm finduse> argument or option as a single
Perl argument. For literal arguments the C<qw()> notation may come in handy.

If you are interested in obtaining certain attributes of for all objects
matching a query as well as their project usage, 
you should look at the L</query_arrayref> and L</query_hashref> methods,
esp. the C<"finduse"> keyword.

Example (recreate the output of the B<ccm finduse> command):

  my $uses = $ccm->finduse(@args);
  foreach my $row (@$uses)
  {
    print shift @$row, "\n";
    if (@$row)
    {
	print "\t$_\n" foreach (@$row);
    }
    else
    {
	print "\tObject is not used in scope.\n";
    }
  }

=cut

sub finduse
{
    my $self = shift;
    my ($rc, $out, $error);

    # NOTE: `ccm finduse ...' without `-query' complains if some of 
    # the given objects do not exist (and exits with status 1 unless at least
    # one exists). But for `ccm finduse -query ...', if there are no hits, 
    # the command exits with status 1 and produces no output on either 
    # stdout and stderr. (This is the same behaviour as for `ccm query ...'.) 
    # We will not produce an error in any case. However, the returned array
    # may contain fewer elements than file_specs given as arguments.
    {
	local $self->{IgnoreError} = 1;
	($rc, $out, $error) = $self->ccm(qw(finduse), @_);
    }

    my (@result, $use);
    foreach (split(/\n/, $out))
    {
	# ignore complaints about non-existing objects 
	# and the dummy "use" line printed if object is not used anywhere
	next if /Object version could not be identified|^\tObject is not used in scope/;

	# a "use" line has the form: 
	# \t project_reference
	push(@$use, $1), next if /^\t(.*)$/;

	# otherwise the line describes an object satisfying the query
	# in the format given by option `Object_format' (default:
	# "%displayname %status %owner %type %project %instance %task")
	push(@result, $use = [ $_ ]);
    }
    return \@result;
}


=item C<findpath>

  $path = $ccm->findpath($file_spec, $proj_vers);

This is a convenience function. It returns the relative pathname for
the object C<$file_spec> within the project C<$proj_vers>. 

Returns C<undef> if C<$file_spec> is not used in C<$proj_vers>
or if C<$file_spec> does not exist.

Example:

  $ccm->findpath("main.c-1:csrc:3", "guilib-darcy"); 
  # returns "guilib/sources/main.c"

=cut

sub findpath
{
    my ($self, $file_spec, $proj_spec) = @_;
    my $finduse = $self->finduse($file_spec);
    return undef unless defined $finduse && @$finduse == 1;

    my $wa_ref_re = qr/^(.*?)\Q$self->{delimiter}\E(?:.*?)\@(.*)$/;
    shift @{ $finduse->[0] };			# trim description
    foreach (@{ $finduse->[0] })
    {
	return $1 if /$wa_ref_re/ && $2 eq $proj_spec;
    }
    return undef;
}
# FIXME proj_spec must be fully qualified, othwerwise no match


=item C<attribute>

  $value = $ccm->attribute($attr_name, $file_spec);
  $old_value = $ccm->attribute($attr_name, $file_spec, $new_value);

Returns the attribute C<$attr_name> for C<$file_spec>.
The attribute value is always returned as a string, regardless
of the CM Synergy type of the attribute.

Returns undef in case of error, e.g. if the attribute
C<$attr_name> doesn't exist.

FIXME set attr (3 arg form)

=cut

sub attribute
{
    my $self = shift;
    return $self->set_error("illegal number of arguments") unless @_ == 2 || @_ == 3;

    my ($attr_name, $file_spec, $new_value) = @_;
    my $old_value;

    # no need to get old value if we are called in void context
    if (defined wantarray)
    {
	my ($rc, $out, $err) = 
	    $self->ccm(qw(attribute -show), $attr_name, $file_spec);
	return undef unless $rc == 0;
	$old_value = $out;	# FIXME "(unset)" possible?
    }

    if (@_ == 3)
    {
	# set attribute
	my $attrs = $self->attributes($file_spec);
	return undef unless $attrs;

	if ($attrs->{$attr_name} =~ /^(up)?text$/)
	{
	    # use ye olde text_editor trick
	    my ($fh, $tempfile) = tempfile();
	    print $fh $new_value;
	    close($fh);

	    my ($rc, $out, $err) = $self->_with_local_option(
		text_editor => "$Config{cp} $tempfile %filename",
	        qw(ccm attribute -modify), $attr_name, $file_spec);

	    unlink($tempfile);

	    return undef unless $rc == 0;
	}
	else
	{
	    my ($rc, $out, $err) = 
		$self->ccm(qw(attribute -modify), $attr_name, 
		           qw(-value), $new_value, $file_spec);
	    return undef unless $rc == 0;
	}
    }

    return $old_value;
}

=item C<attributes>

  $hash_ref = $ccm->attributes($file_spec);

Lists all attributes for C<$file_spec> (i.e. executes B<ccm attr -la>).
Returns a reference to a hash containing pairs of attribute name
and attribute type (e.g. C<string>, C<time>).
Returns C<undef> in case of error.

=cut

sub attributes
{
    my ($self, $file_spec) = @_;
    my ($rc, $out, $err) = $self->ccm(qw(attribute -la), $file_spec);
    return $self->set_error($err || $out) unless $rc == 0;

    my %attrs = $out =~ /^(\S+) \s* \( (.*?) \)/gmx;
    return \%attrs;
}


=item C<types>

  @types = $ccm->types;

B<ccm show -types>

=cut

sub types
{
    my $self = shift;
    my ($rc, $out, $error) = $self->ccm(qw(show -types));
    return undef unless $rc == 0;

    return split(/n/, $out);
}


=item C<ls>

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

=cut

sub ls
{
    my $self = shift;

    my ($rc, $out, $err) = $self->ccm(qw(ls), @_);
    return undef unless $rc == 0;

    return [ split(/\n/, $out) ];
}

=item C<ls_object>

  $ary_ref = $ccm->ls_object($file_spec);

Lists information about a file or the contents of a directory
using the work area name C<$file_spec>.
Returns a reference to an array of corresponding C<VCS::CMSynergy::Object>s.
The default C<$file_spec> is the working directory.

=cut

sub ls_object
{
    my ($self, $file_spec) = @_;
    $file_spec = '.' unless defined $file_spec;

    my $rows = $self->ls(qw(-f %objectname), $file_spec);
    return undef unless $rows;
    return [ map { $self->object($_) } @$rows ];
}


=item C<ls_arrayref>

  $ary_ref = $ccm->ls_arrayref($file_spec, @keywords);

Lists the values of the built-in keywords or attributes supplied
in C<@keywords> for a file or the contents of a directory
using the work area name C<$file_spec>.
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

=cut

sub ls_arrayref
{
    my ($self, $file_spec, @keywords) = @_;
    my $ary_ref = $self->ls_hashref($file_spec, @keywords);
    return unless $ary_ref;
 
    return [ map { [ @$_{@keywords} ] } @$ary_ref ];
}


=item C<ls_hashref>

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

=cut

sub ls_hashref
{
    my ($self, $file_spec, @keywords) = @_;
    return $self->set_error("no file_spec supplied") unless defined $file_spec;
    return $self->set_error("no keywords supplied") unless @keywords;

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


=item C<set>

  $value = $ccm->set($option);
  $old_value = $ccm->set($option, $new_value);
  $hash_ref = $ccm->set;

Get or sets the value of an option.

In the first form it return the value of C<$option>. If the option is unset,
C<undef> is returned (whereas B<ccm set> would print C<"(unset)"> in this case).

In the second form, the C<$option> is set to C<$new_value>, the previous
value is returned. If C<$new_value> is C<undef>, C<$option> is unset.

In the third form, a reference to a hash is returned. The hash consists
of all currently defined options as keys and their respective values.

=cut

sub set
{
    my $self = shift;
    return $self->set_error("too many arguments") unless @_ <= 2;

    my ($rc, $out, $err);
    if (@_ == 0)
    {
	my ($rc, $out, $err) = $self->ccm(qw(set));
	return undef unless $rc == 0;

	my %options;
	while ($out =~ /^(\S+) = (.*)$/gm)
	{
	    $options{$1} = $2 eq "(unset)" ? undef : $2;
	}
	return \%options;
    }

    my ($option, $new_value) = @_;
    my $old_value;

    # no need to get old value if we are called in void context
    if (defined wantarray)
    {
	($rc, $out, $err) = $self->ccm(qw(set), $option);
	return undef unless $rc == 0;
	$old_value = $out eq "(unset)" ? undef : $out;
    }

    if (@_ == 2)
    {
	($rc, $out, $err) = defined $new_value ?
	    $self->ccm(qw(set), $option, $new_value) :
	    $self->ccm(qw(unset), $option);
	return undef unless $rc == 0;
    }
    
    return $old_value;
}
# FIXME: unset method or $ccm->set($option, undef)?


# helper: ccm set foo value; ccm quux ... ; ccm set foo oldvalue
# NOTE: instead of the clumsy ($method, @method_args) I really
# would like to call _with_local_option with a BLOCK as the
# first argument; however, this would rely on a sub prototype and
# prototypes aren't used for method calls :) OTOH _with_local_option
# must be a method because it must manipulate RaiseError/PrinError/error
sub _with_local_option
{
    my ($self, $option, $new_value, $method, @method_args) = @_;
    my (@result, $method_error);

    my $old_value = $self->set($option, $new_value);
    return undef if $self->{error};

    {
	local $self->{IgnoreError} = 1;
	if (wantarray)	{ @result    = $self->$method(@method_args); }
	else            { $result[0] = $self->$method(@method_args); }
	$method_error = $self->{error};	
    }

    $self->set($option, $old_value);
    return undef if $self->{error};

    return $self->set_error($method_error) if $method_error;
    return wantarray ? @result : $result[0];
}


=item C<ps>

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
i.e. C<VCS::CMSynergy->E<gt>C<ps>, as it does not need
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

=cut

sub ps					# class/instance method
{
    my ($this, %filter) = @_;
    my ($rc, $out, $err);

    my @pscmd = qw(ps);
    if (@_ > 1)
    {
	# pass the first "field => value" on to `ccm ps´ 
	# (since `ccm ps -field value' is usually significantly faster
	# than `ccm ps')
	my $field = $_[1];
	push @pscmd, "-$field", $filter{$field};
	delete $filter{$field};
    }
    ($rc, $out, $err) = $this->ccm(@pscmd);
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

    while (my ($field, $value) = each %filter)
    {
	@ps = grep { $_->{$field} eq $value } @ps;
    }

    return \@ps;
}


=item C<databases>

  @databases = $ccm->databases;
  @databases = $ccm->databases($servername);

Returns an array containing the names of all known CM Synergy databases. 

This method can also be called as a class method,
i.e. C<VCS::CMSynergy->E<gt>C<databases>,
as it does not need an established session.

=cut

# FIXME does not work on windows 
# (also not on unix clients that don't have ccmdb_server installed)
sub databases					# class/instance method
{
    my ($this, $servername) = @_;

    my @ccmdb_server = 
    (
	File::Spec->catfile($this->_ccm_home, qw(bin ccmdb_server)),
	'-status'
    );
    push @ccmdb_server, $servername if defined $servername;

    my ($rc, $out, $err) = _ccmexec($this->_ccm_home, @ccmdb_server);
    return ref $this ?
	$this->set_error($err || $out) : undef unless $rc == 0;

    # strip leading/trailing stuff
    $out =~ s/\A.*?^===.*?\n(.*?)\n\n.*\Z/$1/ms;
    return grep { !/dbpath not available/ }
          map  { (split(' ', $_, 3))[2]  } split(/\n/, $out);
}


=item C<version>

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
i.e. C<VCS::CMSynergy->E<gt>C<version> 
as it does not need an established session.

=cut

sub version					# class/instance method
{
    my $this = shift;

    my ($short_version, @full_version) = _version($this->_ccm_home);
    return wantarray ? @full_version : $short_version;
}

# helper (not a method, hence memoizable): 
# returns full version information
sub _version
{
    my ($ccm_home) = @_;

    local $ENV{CCM_HOME} = $ccm_home;	# because we're going to call a class method

    my ($rc, $out, $err) = VCS::CMSynergy->ccm(qw(version -all));
    return undef unless $rc == 0;

    my $cmsynergy = qr/(?:Continuus|CM Synergy)/;
    my ($version, $short_version) = $out =~ /^$cmsynergy Version\s+((\d+\.\d+).*)$/im;
    
    my ($schema) = $out =~ /^$cmsynergy Schema Version\s+(.*)$/im;
    my ($informix) = $out =~ /^Informix.* Version\s+(.*)$/im;
    my ($patches) = $out =~ /^$cmsynergy Patch Version\s+(.*?)(?:\Z|^$cmsynergy|^Informix)/ims; 
    return ($short_version, $version, $schema, $informix, split(/\n/, $patches));
}

memoize('_version');


=item C<status>

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
i.e. C<VCS::CMSynergy->E<gt>C<status>, as it does not need
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


=cut

sub status					# class/instance method
{
    my $this = shift;
    my ($rc, $out, $err) = $this->ccm(qw(status));
    return undef unless $rc == 0;

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
	    # sanitize database path (all other information commands
	    # show it with trailing "/db", so we standardize on that)
	    # FIXME: parsing is broken in case of an NT server
	    ($session->{database} = $1) =~ s{(/db)?$}{/db};
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
# FIXME: this should an optional feature, maybe enabled by some
# 	use VCS::CMSynergy ':wrapper';
sub AUTOLOAD
{
    # NOTE: the fully qualified name of the method has been placed in $AUTOLOAD
    return if $AUTOLOAD =~ /::DESTROY$/;
    
    (my $method = $AUTOLOAD) =~ s/^.*:://;
    $_[0]->trace_msg("autoloading method `$method'\n") if $debug;

    # create the new method on the fly
    no strict 'refs';
    *{$method} = sub { shift->ccm($method, @_) };

    # call it w/o pushing a new stack frame (with same parameters)
    goto &$method;
}


=item C<trace>

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

=cut

sub trace
{
    my ($self, $trace_level, $trace_filename) = @_;
    return $debug unless defined $trace_level;
    ($debug, $trace_level) = ($trace_level, $debug);
    if (@_ == 3)				# $trace_filename present
    {
	# switch trace files
	my $newfh = defined $trace_filename ?
	    new IO::File($trace_filename, "a") : \*STDERR;
	unless ($newfh)
	{
	    carp(__PACKAGE__ . " trace: can't open trace file `$trace_filename'");
	    return $trace_level;
	}
	$newfh->setvbuf(undef, _IOLBF, 0);	# make line-buffered
	close($debugfh) if defined $debugfh && $debugfh != \*STDERR;
	$debugfh = $newfh;
    }
    return $trace_level;
}

=item C<trace_msg>

  $ccm->trace_msg($message_text);
  $ccm->trace_msg($message_text, $min_level);

Writes C<$message_text> to the trace file if trace is enabled.
Can also be called as C<VCS::CMSynergy->E<gt>C<trace_msg($msg)>.
See L</trace>.

If C<$min_level> is defined, then the message is output only if the trace
level is equal to or greater than that level. C<$min_level> defaults to 1.

=cut

sub trace_msg
{
    my ($self, $message, $min_level) = @_;
    $min_level ||= 1;
    print $debugfh "[$self] $message" if $debug >= $min_level;
}


=item C<use_ccm_coprocess>

  VCS::CMSynergy->use_ccm_coprocess;

This feature is highly experimental, B<use it at your own risk>.
You must have the L<Expect> module installed to use this feature.
(Since L<Expect> is not available for Win32 systems, 
this feature cannot be used there.)

In its default operating mode, C<VCS::CMSynergy.pm> invokes a separate
C<ccm> process for most simple methods, e.g. 

  $ccm->checkout('foo.c');
  $ccm->checkout('bar.c');

results in the execution of

  ccm checkout foo.c
  ccm checkout bar.c

Hence we incur the startup overhead of B<ccm> for most method 
invocations. This overhead is noticable, esp. if you are doing 
lots of CM Synergy operations.

If C<use_ccm_coprocess> is in effect, only one ccm process per CM Synergy
session is used. C<VCS::CMSynergy->new> starts an "interactive" B<ccm> process,
funnels commands to its input and reads back the output
(up to the next C<"ccmE<gt>"> prompt). This avoids the startup
overhead, but may run into other problems, e.g. restrictions
on the length of the B<ccm> command line.

=cut

sub use_ccm_coprocess			# class method
{
    my $class = shift;
    # NOTE: Expect versions 1.11 and 1.13 have a different interface for
    # setting things up. Also, 1.13 drops the `exp_' prefix on most methods
    # (though the old names are still recognized).
    # 
    ### 1.11
    # my $exp = Expect->spawn(...);
    # $exp->exp_stty('-echo raw');		# single parameter, modes separeted by whitespace
    # $exp->log_stdout(0);
    # ...
    # $exp->exp_before...
    # $exp->exp_error...
    ### 1.15
    # my $exp = new Expect;
    # $exp->log_stdout(0);
    # $exp->slave->stty(qw(raw -echo));		# paramter list of modes
    # $exp->spawn(...);
    # ...
    # $exp->before...
    # $exp->error...
    eval { require Expect; import Expect 1.15; };
    croak(__PACKAGE__ . " use_ccm_coprocess: $@") if $@;

    $use_ccm_coprocess = 1;
}


# (from DBI):
# $error	new error message
# $method	alternative merthod name (instead of caller's name)
# $rv		return value from set_error (in scalar or void context)
# @rv		return value in list context (if given)
sub set_error 
{
    my ($this, $error, $method, $rv, @rv) = @_;

    ref $this ? $this->{error} : $VCS::CMSynergy::error = $error;

    my ($package, undef, undef, $subroutine) = caller(1);
    ($method) = $subroutine =~ /::(.*?)$/ unless defined $method;

    if (ref $this)
    {
	# try the HandleError routine if one was provided;
	# consider the error handled if it returns true
	# NOTE: the handler may change the value of $rv
	my $handler = $this->{HandleError};
	return 1 if $handler and &$handler($error, $this, $rv);

	my $msg = "$package $method: $error";
	croak($msg) if $this->{RaiseError};	# die if RaiseError is set
	carp($msg)  if $this->{PrintError};	# warn if PrintError is set
    }

    return @rv if wantarray and @rv;
    return $rv;
}

# $ccm->object(objectname) => VCS::CMSynergy::Object
# $ccm->object(name, version, cvtype, instance) => VCS::CMSynergy::Object
sub object
{
    my $self = shift;

    return $self->set_error("illegal number of arguments")
	unless @_ == 1 || @_ == 4;
    
    return VCS::CMSynergy::Object->new(@_, $self->{delimiter}) if @_ == 4;

    # Sigh. "ccm query -f %objectname" returns old-style fullnames
    # ("instance/cvtype/name/version") for certain types of objects
    # (e.g. "cvtype" and "attype").
    my ($name, $version, $cvtype, $instance);
    local $_ = $_[0];
    return VCS::CMSynergy::Object->new($name, $version, $cvtype, $instance, $self->{delimiter})
	if ($name, $version, $cvtype, $instance) = m{$self->{objectname_re}}
	or ($instance, $cvtype, $name, $version) = m{^(.*?)/(.*?)/(.*?)/(.*?)$};

    return $self->set_error("invalid objectname `$_'");
}

=back

=head1 TODO

=over 4

=item *

anything else?

=back

=head1 AUTHORS

Roderich Schupp, argumentum GmbH E<lt>F<schupp@argumentum.de>E<gt>

=head1 COPYRIGHT

Copyright (c) 2001-2002 argumentum GmbH, F<http://www.argumentum.de>.
All rights reserved.

You may distribute it under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=cut

package VCS::CMSynergy::Object;

use Carp;

use overload '""' => \&objectname;

# VCS::CMSynergy::Object->new(name, version, cvtype, instance, delimiter)
sub new
{
    my $class = shift;
    unless (@_ == 5)
    {
	carp(__PACKAGE__ . " new: illegal number of arguments");
	return undef;
    }
    my $delim = pop @_;
    return bless [ @_, "$_[0]${delim}$_[1]:$_[2]:$_[3]" ], $class;
}

sub name	{ return shift->[0]; }
sub version	{ return shift->[1]; }
sub cvtype	{ return shift->[2]; }
sub instance	{ return shift->[3]; }
sub objectname	{ return shift->[4]; }

sub proj_vers	
{ 
    my $self = shift;
    carp(__PACKAGE__ . " proj_vers: not a project: $self") unless $self->cvtype eq 'project';
    (my $proj_vers = $self->objectname) =~ s/:.*//;
    return $proj_vers;
}

1;
