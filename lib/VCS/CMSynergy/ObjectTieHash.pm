package VCS::CMSynergy::ObjectTieHash;

# Copyright (c) 2001-2010 argumentum GmbH, 
# See COPYRIGHT section in VCS/CMSynergy.pod for usage and distribution rights.

our $VERSION = do { (my $v = q$Revision$) =~ s/^.*:\s*//; $v };

use base 'VCS::CMSynergy::Object';

# TIEHASH(class, { ccm => ..., name => ..., ...})
sub TIEHASH
{
    my ($class, $href) = @_;
    return bless $href, $class;
}

sub FETCH
{
    my ($self, $key) = @_;
    return $self->get_attribute($key);
}

sub STORE
{
    my ($self, $key, $value) = @_;
    return $self->set_attribute($key, $value);
}

sub EXISTS
{
    my ($self, $key) = @_;
    return defined $self->set_attribute($key);
}

sub FIRSTKEY
{
    my ($self) = @_;
    $attrs = $self->list_attributes;
    $self->_private->{_attrs} = [ keys %{ $self->list_attributes } ];
    return pop @{ $self->_private->{_attrs} };
}

sub NEXTKEY
{
    my ($self, $lastkey) = @_;
    return pop @{ $self->_private->{_attrs} };
}


# redefine getter methods and access to private data
{
    package VCS::CMSynergy::Object;

    no strict 'refs';
    no warnings 'redefine';

    foreach my $method (qw(objectname ccm name version cvtype instance))
    {
	*{$method} = sub { my $self = shift; (tied %$self || $self)->{$method}; };
    }

    # access private parts via the tied object
    # FIXME NOTE why the || below? because in FETCH etc we are NOT tied
    # but otherwise we ARE
    sub _private	{ my $self = shift; tied %$self || $self; }
}


1;
