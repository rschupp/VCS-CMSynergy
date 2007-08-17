#!/usr/bin/perl

# List all objects with parallel versions within a given project.
# Optionally report finduse information.


# Usage: find_parallel_versions.pl [-v] database proj_vers


use Getopt::Std;
use VCS::CMSynergy;

our ($opt_v);
getopts('v');
die "usage: $0 database proj_vers" unless @ARGV == 2;
my ($database, $proj_vers) = @ARGV;

my $ccm = VCS::CMSynergy->new(
    database	=> $database,
    RaiseError	=> 1,
);

foreach my $member (@{ $ccm->query_object("is_member_of('$proj_vers')") })
{
    my ($name, $instance, $cvtype) = ($member->name, $member->instance, $member->cvtype);

    my $parallel_versions = $ccm->query_object(
	qq[name='$name' and instance='$instance' and cvtype='$cvtype' and is_hist_leaf()]);
    if (@$parallel_versions > 1)
    {
	print "$name:$cvtype:$instance\n";
	next unless $opt_v;
	foreach my $object (@$parallel_versions)
	{
	    print "\tversion ", $object->version, "\n";
	    my ($finduse) = @{ $ccm->finduse($object) };
	    while (my ($pr, $fu) = each %{ $finduse->[1] })
	    {
		print "\t\tused in $pr as $fu\n";
	    }
	}
    }
}
