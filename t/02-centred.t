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
my $w = 100;
my $h = 100;

my $gbb1 = Geo::BoundingBox->new();
ok(defined $gbb1, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb1->centred_at(@centre, $w), "bounded_by(): called"); $num_tests++;

my $gbb2 = Geo::BoundingBox->new();
ok(defined $gbb2, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb2->centred_at(@centre, $w, $h), "bounded_by(): called"); $num_tests++;

my $gbb3 = Geo::BoundingBox->new();
ok(defined $gbb3, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb3->centred_at([@centre, $w]), "bounded_by(): called"); $num_tests++;

my $gbb4 = Geo::BoundingBox->new();
ok(defined $gbb4, "Geo::BoundingBox->new() : called") or BAIL_OUT("call to Geo::BoundingBox->new() has failed"); $num_tests++;
ok(1==$gbb4->centred_at([@centre, $w, $h]), "bounded_by(): called"); $num_tests++;

ok(1==$gbb1->equals($gbb2, 6), "equals(): called"); $num_tests++;
ok(1==$gbb1->equals($gbb3, 6), "equals(): called"); $num_tests++;
ok(1==$gbb1->equals($gbb4, 6), "equals(): called"); $num_tests++;

# END
done_testing($num_tests);
