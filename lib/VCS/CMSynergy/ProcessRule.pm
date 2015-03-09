package VCS::CMSynergy::ProcessRule;

# Copyright (c) 2001-2015 argumentum GmbH
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

use strict;
use warnings;

=head1 NAME

VCS::CMSynergy::ProcessRule - convenience methods for C<VCS::CMSynergy::Object>s of type I<process_rule>

=head1 SYNOPSIS

C<VCS::CMSynergy::ProcessRule> is a subclass of 
L<C<VCS::CMSynergy::Object>|VCS::CMSynergy::Object>
with additional methods for Synergy I<process_rules>.

  use VCS::CMSynergy;
  $ccm = VCS::CMSynergy->new(%attr);
  ...
  $proj = $ccm->object("editor-1:project:1");
  $pr = $proj->process_rule();

  $projects = $pr->show_object("baseline_projects");

=cut 

use base qw(VCS::CMSynergy::Object);

use Carp;
use File::Spec;
use Cwd;

=head1 METHODS

=head2 show

  $aref = $pr->show_hashref($what, @keywords);
  $aref = $pr->show_object($what, @keywords);

These two methods are convenience wrappers for 
B<ccm process_rule -show $what>. For return values and the
meaning of the optional C<@keywords> parameters see the descriptions
of L<query_hashref|VCS::CMSynergy/"query_arrayref, query_hashref"> 
and L<query_object|VCS::CMSynergy/query_object>.

The following strings can be used for C<$what>, see the Synergy documentation
of the B<ccm process_rule -show> sub command for their meaning:

=over 5

=item *
baseline_projects

=item *
folders

=item *
folder_templates

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
            qw/process_rule -u -ns -nch -nf/,
            -show   => $what,
            -format => $format, $self);
    return $self->set_error($err || $out) unless $rc == 0;

    # split $out at $RS and ignore the first element
    # (which is either empty or a header "Process Rule...:")
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
