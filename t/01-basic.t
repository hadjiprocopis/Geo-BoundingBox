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
is(int($gbb->min_lat()),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($gbb->min_lon()),int($centre[1]), "checking latitude conventions"); $num_tests++;
is(int($gbb->max_lat()),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($gbb->max_lon()),int($centre[1]), "checking latitude conventions"); $num_tests++;
my $bbox = $gbb->as_LATLON_bbox();
is(int($bbox->[0]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[1]),int($centre[1]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[2]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[3]),int($centre[1]), "checking latitude conventions"); $num_tests++;
$bbox = [split /\s*,\s*/, $gbb->stringify_as_LATLON_bbox()];
is(int($bbox->[0]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[1]),int($centre[1]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[2]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($bbox->[3]),int($centre[1]), "checking latitude conventions"); $num_tests++;
my $osm_bbox = $gbb->as_OSM_bbox();
is(int($osm_bbox->[1]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[0]),int($centre[1]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[3]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[2]),int($centre[1]), "checking latitude conventions"); $num_tests++;
$osm_bbox = [split /\s*,\s*/, $gbb->stringify_as_OSM_bbox()];
is(int($osm_bbox->[1]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[0]),int($centre[1]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[3]),int($centre[0]), "checking latitude conventions"); $num_tests++;
is(int($osm_bbox->[2]),int($centre[1]), "checking latitude conventions"); $num_tests++;

my $gbb2 = Geo::BoundingBox->new();
ok(defined $gbb2, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb2->bounded_by([ @{$gbb->as_LATLON_bbox()} ]), "bounded_by(): called"); $num_tests++;
ok(1==$gbb->equals($gbb2, 5), "equals(): called"); $num_tests++;
ok(1==$gbb2->equals($gbb, 5), "equals(): called"); $num_tests++;

my @smaller_bbox_array = (
	$centre[0]-0.005, $centre[1]-0.005, # that's +- 5km
	$centre[0]+0.005, $centre[1]+0.005
);
my $gbb3 = Geo::BoundingBox->new();
ok(defined $gbb3, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb3->bounded_by(\@smaller_bbox_array), "bounded_by(): called"); $num_tests++;
ok(1==$gbb->encloses($gbb3, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb3->encloses($gbb, 5), "encloses(): called"); $num_tests++;

my @bigger_bbox_array = (
	$centre[0]-0.015, $centre[1]-0.015, # that's +- 15km
	$centre[0]+0.015, $centre[1]+0.015
);
my $gbb4 = Geo::BoundingBox->new();
ok(defined $gbb4, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb4->bounded_by(\@bigger_bbox_array), "bounded_by(): called"); $num_tests++;
ok(0==$gbb->encloses($gbb4, 5), "encloses(): called"); $num_tests++;
ok(1==$gbb4->encloses($gbb, 5), "encloses(): called"); $num_tests++;

my $gbb100 = Geo::BoundingBox->new();
ok(defined $gbb100, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
# make a square bbox of side 100m centred at @centre
ok(1==$gbb100->centred_at([@centre, 100]), "centred_at(): called"); $num_tests++;
ok(1==$gbb->encloses($gbb100, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb100->encloses($gbb, 5), "encloses(): called"); $num_tests++;

my $gbb90 = Geo::BoundingBox->new();
ok(defined $gbb90, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
# make a square bbox of side 90m centred at @centre
ok(1==$gbb90->centred_at([@centre, 90]), "centred_at(): called"); $num_tests++;
ok(1==$gbb->encloses($gbb90, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb90->encloses($gbb, 5), "encloses(): called"); $num_tests++;

my $gbb110 = Geo::BoundingBox->new();
ok(defined $gbb110, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
# make a square bbox of side 110m centred at @centre
ok(1==$gbb110->centred_at([@centre, 110]), "centred_at(): called"); $num_tests++;
ok(1==$gbb->encloses($gbb110, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb110->encloses($gbb, 5), "encloses(): called"); $num_tests++;

ok(1==$gbb110->encloses($gbb100, 5), "encloses(): called"); $num_tests++;
ok(1==$gbb110->encloses($gbb90, 5), "encloses(): called"); $num_tests++;
ok(1==$gbb100->encloses($gbb90, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb100->encloses($gbb110, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb90->encloses($gbb100, 5), "encloses(): called"); $num_tests++;
ok(0==$gbb90->encloses($gbb110, 5), "encloses(): called"); $num_tests++;

my $gbb5 = Geo::BoundingBox->new();
my $str = $centre[0].':'.$centre[1].',100';
ok(1==$gbb5->centred_at($str), "centred_at(): called"); $num_tests++;
ok($gbb5->equals($gbb100), "centred_at(): checking"); $num_tests++;
$str = $centre[0].':'.$centre[1].',100x100';
ok(1==$gbb5->centred_at($str), "centred_at(): called"); $num_tests++;
ok($gbb5->equals($gbb100), "centred_at(): checking"); $num_tests++;
$str = $gbb100->stringify_as_LATLON_bbox();
ok(1==$gbb5->bounded_by($str), "centred_at(): called"); $num_tests++;
ok($gbb5->equals($gbb100), "centred_at(): checking"); $num_tests++;
$str = $gbb100->stringify_as_LATLON_bbox(); $str =~ s/,/:/g;
ok(1==$gbb5->bounded_by($str), "centred_at(): called"); $num_tests++;
ok($gbb5->equals($gbb100), "centred_at(): checking"); $num_tests++;

my $gbb7 = Geo::BoundingBox->new();
ok(1==$gbb7->bounded_by(\@bbox_array), "bounded_by(): called"); $num_tests++;

my $gbb6 = Geo::BoundingBox->new();
$str = $bbox_array[0].':'.$bbox_array[1].','.$bbox_array[2].':'.$bbox_array[3];
ok(1==$gbb6->bounded_by($str), "bounded_by(): called"); $num_tests++;
ok(0==$gbb6->equals($gbb5), "bounded_by(): checking"); $num_tests++;
$str =~ s/,/:/g;
ok(1==$gbb6->bounded_by($str), "bounded_by(): called"); $num_tests++;
ok($gbb6->equals($gbb7), "bounded_by(): checking"); $num_tests++;

$str = $bbox_array[0].':'.$bbox_array[1].','.$bbox_array[2].':'.$bbox_array[3];
$str =~ s/\:/,/g;
ok(1==$gbb6->bounded_by($str), "bounded_by(): called"); $num_tests++;
ok($gbb6->equals($gbb7), "bounded_by(): checking"); $num_tests++;

# END
done_testing($num_tests);
