package VCS::CMSynergy::ObjectTieHash;

use Scalar::Util qw(weaken);

# TIEHASH(class, { ccm => ..., name => ..., ...})
sub TIEHASH
{
    my ($class, $href) = @_;
    return bless $href, $class;
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
    $self->{attributes} ||= $self->{ccm}->list_attributes($self->{objectname});

    return exists $self->{attributes}->{$key};
}

sub FIRSTKEY
{
    my ($self) = @_;
    $self->{attributes} ||= $self->{ccm}->list_attributes($self->{objectname});

    my $dummy = keys %{ $self->{attributes} };	# reset each() iterator
    return each %{ $self->{attributes} };
}

sub NEXTKEY
{
    my ($self, $lastkey) = @_;
    $self->{attributes} ||= $self->{ccm}->list_attributes($self->{objectname});

    return each %{ $self->{attributes} };
}

sub DELETE
{
    my ($self, $key) = @_;
    $self->{ccm}->delete_attribute($key);
}


package VCS::CMSynergy::Object;

sub get_attribute
{
    my ($self, $attr_name) = @_;
    #(tied %$self)->{ccm}->get_attribute($attr_name, $self);
    (tied %$self)->FETCH($attr_name);
}

sub set_attribute
{
    my ($self, $attr_name, $value) = @_;
    #(tied %$self)->{ccm}->set_attribute($attr_name, $self, $value);
    (tied %$self)->STORE($attr_name, $value);
}

sub list_attributes
{
    my ($self) = @_;
    my $tied = tied %$self;

    $tied->{attributes} ||= $tied->{ccm}->list_attributes($self);
}

sub create_attribute
{
    my ($self, $name, $type, $value) = @_;
    my $tied = tied %$self;
    
    my $rc = $tied->{ccm}->create_attribute($name, $type, $value, $self);
    $tied->{attributes}->{$name} = $type if $rc && $tied->{attributes};
    return $rc;
}

sub delete_attribute
{
    my ($self, $name) = @_;
    my $tied = tied %$self;

    my $rc = $tied->{ccm}->delete_attribute($name, $self);
    delete $tied->{attributes}->{$name} if $rc && $tied->{attributes};
    return $rc;
}

sub copy_attribute
{
    my ($self, $name, $flags, @to_file_specs) = @_;
    (tied %$self)->{ccm}->copy_attribute($name, $flags, $self, @to_file_specs);
}

sub property
{
    my ($self, $keyword) = @_;
    (tied %$self)->{ccm}->property($keyword, $self);
}

sub displayname
{
    my ($self) = @_;
    (tied %$self)->{displayname} ||= $self->property('displayname');
}


1;

