#!/usr/bin/perl

use strict;
use Test::More;

eval "use Test::Pod::Coverage 1.00";
plan $@ ?
    (skip_all => "Test::Pod::Coverage 1.00 required for testing pod coverage") :
    (tests => 9);

pod_coverage_ok("VCS::CMSynergy",                   
    { trustme => [qw(query_object_with_attributes)] }); # obsolete
pod_coverage_ok("VCS::CMSynergy::Client",
    { also_private => [ qr/^ccm$/, qr/^start$/ ] });
pod_coverage_ok("VCS::CMSynergy::Helper");
pod_coverage_ok("VCS::CMSynergy::Object",
    { trustme => [qw(show_object show_hashref)] }); # documented elsewhere
pod_coverage_ok("VCS::CMSynergy::Baseline");
pod_coverage_ok("VCS::CMSynergy::Project");
pod_coverage_ok("VCS::CMSynergy::ProjectGrouping");
pod_coverage_ok("VCS::CMSynergy::ProcessRule");
pod_coverage_ok("VCS::CMSynergy::Users");

