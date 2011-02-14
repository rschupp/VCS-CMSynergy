#line 1
package End;

use 5.006;

use strict;
use warnings;
no  warnings 'syntax';

use Exporter;

our @ISA     = qw /Exporter/;
our @EXPORT  = qw /end/;

our $VERSION = '2009040201';

sub end (&) {
    my    $code =  shift;
    # Due to a bug in Perl 5.6.0, we can't just bless $code.
    # But by creating an extra closure, it'll work.
    bless sub {$code -> ()} => __PACKAGE__;
}

DESTROY {$_ [0] -> ()}

1;

__END__

#line 107
