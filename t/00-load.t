#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Geo::BoundingBox' ) || print "Bail out!\n";
}

diag( "Testing Geo::BoundingBox $Geo::BoundingBox::VERSION, Perl $], $^X" );
