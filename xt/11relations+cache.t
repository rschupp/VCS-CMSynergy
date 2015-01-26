#!/usr/bin/perl -w

# rerun 09attribute.t with cached_attributes 
$ENV{CCM_USE_CACHED_ATTRIBUTES} = 1;
do 'xt/11relations.t' or warn $!;
die if $@;
exit 0;
