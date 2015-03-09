package VCS::CMSynergy::ProjectGrouping;

# Copyright (c) 2001-2015 argumentum GmbH
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

use strict;
use warnings;

=head1 NAME

VCS::CMSynergy::ProjectGrouping - convenience methods for C<VCS::CMSynergy::Object>s of type I<project_grouping>

=head1 SYNOPSIS

C<VCS::CMSynergy::ProjectGrouping> is a subclass of 
L<C<VCS::CMSynergy::Object>|VCS::CMSynergy::Object>
with additional methods for Synergy I<project_groupings>.

  use VCS::CMSynergy;
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  $proj = $ccm->object("editor-1:project:1");
  $pg = $proj->project_grouping();

  $projects = $pg->show_object("projects");
  $tasks = $pg->show_object(
    tasks_on_top_of_baseline => qw( task_synopsis completion_date ));

=cut 

use base qw(VCS::CMSynergy::Object);

use Carp;
use File::Spec;
use Cwd;

=head1 METHODS

=head2 show

  $aref = $pg->show_hashref($what, @keywords);
  $aref = $pg->show_object($what, @keywords);

These two methods are convenience wrappers for 
B<ccm project_grouping -show $what>. For return values and the
meaning of the optional C<@keywords> parameters see the descriptions
of L<query_hashref|VCS::CMSynergy/"query_arrayref, query_hashref"> 
and L<query_object|VCS::CMSynergy/query_object>.

The following strings can be used for C<$what>, see the Synergy documentation
of the B<ccm project_grouping -show> sub command for their meaning:

=over 5

=item *
added_tasks

=item *
all_tasks

=item *
automatic_tasks

=item *
baseline

=item *
folders

=item *
objects

=item *
projects

=item *
removed_tasks

=item *
tasks_on_top_of_baseline

=back

=cut

sub _show
{
    my ($self, $what, $keywords, $row_type) = @_;

    croak(__PACKAGE__."::show_{hashref,object} are only available in web mode")
        unless $self->ccm->web_mode;

    my $want = VCS::CMSynergy::_want($row_type, $keywords);
    my $format = $VCS::CMSynergy::RS . join($VCS::CMSynergy::FS, values %$want) . $VCS::CMSynergy::FS;

    my ($rc, $out, $err) = $self->ccm->ccm( 
            qw/project_grouping -u -ns -nch -nf/,
            -show   => $what,
            -format => $format, $self);
    return $self->set_error($err || $out) unless $rc == 0;

    # split $out at $RS and ignore the first element
    # (which is either empty or a header "Project Grouping ...:")
    my (undef, @records) = split(/\Q${VCS::CMSynergy::RS}\E/, $out);

    my @result;
    foreach (@records)
    {
        my @cols = split(/\Q${VCS::CMSynergy::FS}\E/, $_, -1);    # don't strip empty trailing fields
        my $row = $self->ccm->_query_result($want, \@cols, $row_type);
        push @result, $row;
    }
    return \@result;
}

1;
