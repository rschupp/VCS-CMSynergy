package VCS::CMSynergy::ObjectTieHash;

use Scalar::Util qw(weaken);

# TIEHASH(class, ccm, name, version, cvtype, instance)
sub TIEHASH
{
    my $class = shift;
    my $self = {};

    @$self{qw(ccm name version cvtype instance)} = @_;
    weaken $self->{ccm};

    my $delim = shift->delimiter;
    $self->{objectname} = "$_[0]${delim}$_[1]:$_[2]:$_[3]";

    return bless $self, $class;
}

sub FETCH
{
    my ($self, $key) = @_;
    return $self->{ccm}->get_attribute($key, $self->{objectname});
}

sub STORE
{
    my ($self, $key, $value) = @_;
    return $self->{ccm}->set_attribute($key, $self->{objectname}, $value);
}

sub EXISTS
{
    my ($self, $key) = @_;
    $self->list_attributes unless $self->{attributes};

    return exists $self->{attributes}->{$key};
}

sub FIRSTKEY
{
    my ($self) = @_;
    $self->list_attributes unless $self->{attributes};

    my $dummy = keys %{ $self->{attributes} };	# reset each() iterator
    return each %{ $self->{attributes} };
}

sub NEXTKEY
{
    my ($self, $lastkey) = @_;
    $self->list_attributes unless $self->{attributes};

    return each %{ $self->{attributes} };
}

sub DELETE
{
    my ($self, $key) = @_;
    $self->delete_attribute($key);
}

sub list_attributes
{
    my ($self) = @_;
    $self->{attributes} ||= $self->{ccm}->list_attributes($self->{objectname});
}

sub create_attribute
{
    my ($self, $name, $type, $value) = @_;
    
    my $rc = $self->{ccm}->create_attribute($name, $type, $value, $self->{objectname});
    $self->{attributes}->{$name} = $type if $rc && $self->{attributes};
    return $rc;
}

sub delete_attribute
{
    my ($self, $name) = @_;

    my $rc = $self->{ccm}->delete_attribute($name, $self->{objectname});
    delete $self->{attributes}->{$name} if $rc && $self->{attributes};
    return $rc;
}

sub copy_attribute
{
    my ($self, $name, $flags, @to_file_specs) = @_;
    return $self->{ccm}->copy_attribute($name, $flags, $self->{objectname}, @to_file_specs);
}

sub property
{
    my ($self, $keyword) = @_;
    return $self->{ccm}->property($keyword, $self->{objectname});
}

sub displayname
{
    my ($self) = @_;
    $self->{displayname} ||= $self->property('displayname');
}


1;

