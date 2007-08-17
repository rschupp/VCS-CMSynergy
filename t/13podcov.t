#!/usr/bin/perl

use strict;
use Test::More;

eval "use Test::Pod::Coverage 1.00";
plan $@ ?
    (skip_all => "Test::Pod::Coverage 1.00 required for testing pod coverage") :
    (tests => 5);

pod_coverage_ok("VCS::CMSynergy");
pod_coverage_ok("VCS::CMSynergy::Client", 
                { also_private => [ qw/ccm exec start/ ] });
pod_coverage_ok("VCS::CMSynergy::Helper");
pod_coverage_ok("VCS::CMSynergy::Object");
pod_coverage_ok("VCS::CMSynergy::Users");

