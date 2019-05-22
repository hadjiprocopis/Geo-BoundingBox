#!/usr/bin/env perl

use strict;
use warnings;

use lib 'blib/lib';

use Test::More;

use Geo::BoundingBox;
use Geo::Calc;

my $num_tests = 0;
my @centre = (
	35.170985, # lat
	33.357755  # lon
);
my @bbox_array = (
	$centre[0]-0.01, $centre[1]-0.01, # that's +- 10km
	$centre[0]+0.01, $centre[1]+0.01
);
my $gbb = Geo::BoundingBox->new();
ok(defined $gbb, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb->bounded_by(\@bbox_array), "bounded_by(): called"); $num_tests++;

my $gbb2 = Geo::BoundingBox->new();
ok(defined $gbb2, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb2->bounded_by(@bbox_array), "bounded_by(): called"); $num_tests++;

ok(1==$gbb->equals($gbb2, 6), "equals(): called"); $num_tests++;

# END
done_testing($num_tests);
