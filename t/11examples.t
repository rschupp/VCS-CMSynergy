#!/usr/bin/perl

use Test::More tests => 8;
use t::util;
use strict;

use IPC::Run3;

sub run_perl
{
    my $out;

    run3 [ $^X, (map { "-I$_" } @INC), '-w', @_ ],
	\undef, \$out;

    return wantarray ? ($?, $out) : $?;
}


BEGIN { use_ok('VCS::CMSynergy'); }

my $ccm = VCS::CMSynergy->new(%::test_session);
isa_ok($ccm, "VCS::CMSynergy");
diag("using coprocess") if defined $ccm->{coprocess};
$ENV{CCM_ADDR} = $ccm->ccm_addr;

is(run_perl(qw(-c examples/project_diff)), 0, q[examples/project_diff: compile it]);

SKIP: 
{
    my $file_exp = File::Spec->catfile("t", "project_diff-" . $ccm->version . ".txt");
    skip "can't find expected output for project_diff test ($file_exp) for this version of CM Synergy", 2
	unless -e $file_exp;
    
    my $exp = do
    {
	local $/ = undef;
	open my $fh, "<$file_exp" or die "can't open $file_exp: $!";
	local $_ = <$fh>;
	close $fh;
	s:\015\012:\012:g;		# normalize newlines
	$_;
    };

    my ($rc, $got) = run_perl(qw(examples/project_diff -r toolkit-1.0 toolkit-darcy));
    is($rc, 1 << 8, q[examples/project_diff: exit status 1]);

    $got =~ s:\015\012:\012:g;		# normalize newlines
    $got =~ s:\\:/:g;			# normalize pathnames
    is($got, $exp, q[examples/project_diff: compare output]);
}

is(run_perl(qw(-c examples/grep_attr)), 0, q[examples/grep_attr: compile it]);
{
    chomp(my $exp = << 'EOF');		# must be sorted
task25-1:task:probtrac:sandbox needs to support rgb color settings
task28-1:task:probtrac:Add rgb handling code from the sandbox team to bufcolor.c for the play project
EOF
	
    my ($rc, $got) = run_perl(qw(examples/grep_attr --ignore-case rgb task_synopsis --query), "type = 'task'");
    $got = join("\n", sort split(/\n/, $got));
    is($rc, 0, q[examples/grep_attr: exit status 1]);
    is($got, $exp, q[examples/grep_attr: compare output]);
}

exit 0;
