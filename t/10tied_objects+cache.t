#!/usr/bin/perl -w

# rerun 10tied_objects.t with cached_attributes 
$ENV{CCM_USE_CACHED_ATTRIBUTES} = 1;
do 't/10tied_objects.t' or warn $!;
die if $@;
exit 0;

