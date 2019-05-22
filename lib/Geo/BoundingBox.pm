package Geo::BoundingBox;

use 5.006;
use strict;
use warnings;

our $VERSION = '0.01';

use Exporter qw(import);
our @EXPORT = qw/
	parse_LATLON_bbox_string
	parse_centred_at_string
/;

######
# NOTE:
# The heavy calculations are copied verbatim from [mod://Geo::Calc]
# in order to keep it lightweight and avoid moose baggage, sorry.
#
# IMPORTANT:
# sub input params convention is lat,lon (google uses the same)
# even bounded_by() takes LAT,LON
# printing is also LAT,LON unless you specify OSM
######

use Math::Trig qw(:pi asin acos tan deg2rad rad2deg);
use POSIX qw(modf fmod);

use overload
	'""' => 'stringify_as_LATLON_bbox'
	#'""' => 'stringify_as_OSM_bbox'
;

sub new {
	my $class = $_[0];
	my $params = $_[1];
	$params = {} unless defined $params;
	my $self = {
		# we are storing internally a bbox as left-bottom(lat,lon), top-right(lat,lon)
		# the convention is LAT,LON and this is what we use in this internal storage
		# however, OSM uses LON,LAT and we will reverse when asked to dump OSM
		'bbox' => []
	};
	return bless $self, $class
}
# checks if bbox was set with coordinates
sub was_set { return scalar @{$_[0]->{'bbox'}} == 4 }

# sets the bounding box given a centre (as lat,lon) <<< LAT,LON convention
# a box width and optionally a box height (otherwise makes a square box)
# the input is an arrayref or array of 3 or 4 items.
# the first two are the LAT,LON of the centre
# third is the width
# optional fourth is the height. If no height is specified, then the bounding box is a square.
# returns 1 on success
# returns 0 on failure
# Note: it relies on Geo::BoundingBox::_boundry_box() which is copied from Geo::Calc's boundry_box()
# (by Sorin Alexandru Pop) (see https://metacpan.org/pod/Geo::Calc#boundry_box)
sub centred_at {
	my $self = $_[0];
	my $m = $_[1];

	my $parent = ( caller(1) )[3] || "N/A";
	my $whoami = ( caller(0) )[3];

	my $rm = ref $m;
	if( $rm eq 'ARRAY' and @$m >= 3 ){
		# we are given an arrayref of 3 or 4 items as input
		# forms a square box centred on long,lat with width and optional height
		my $bb = Geo::BoundingBox::_boundry_box(
			$m->[0], $m->[1], # centre LAT,LON
			$m->[2], # width
			defined $m->[3] ? $m->[3] : $m->[2], # optional height or square
		);
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, @$bb
	} elsif( $rm eq '' and @_ >= 4 ){
		# we are given an array of 3 or 4 items as input (note 3+1=4)
		# forms a square box centred on long,lat with width and optional height
		my $bb = Geo::BoundingBox::_boundry_box(
			$_[1], $_[2], # centre LAT,LON
			$_[3], # width
			defined $_[4] ? $_[4] : $_[3], # optional height or square
		);
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, @$bb
	} elsif( $rm eq '' and @_ == 2 ){
		# we are given a string containing the centred-at spec
		my $spec = Geo::BoundingBox::parse_centred_at_string($m);
		if( ! defined $spec ){ print STDERR "$whoami (via $parent) : call to ".'Geo::BoundingBox::parse_centred_at_string()'." has failed for input string '$m'.\n"; return 0 }
		my $bb = Geo::BoundingBox::_boundry_box(
			$spec->[0], $spec->[1], # centre LAT,LON
			$spec->[2], # width
			defined $spec->[3] ? $spec->[3] : $spec->[4], # optional height or square
		);
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, @$bb
	} else {
		print STDERR "Geo::BoundingBox::centred_at() : it accepts [centre-lon,centre-lat,width and optional height].\n";
		return 0
	}
	return 1 # success
}
# sets the bounding box given an array specifying a bounding box
# following the convention of left-bottom(LAT,LON), right-top(LAT,LON)
# (this is not like OSM's bbox which is LON,LAT, but is the same wrt to left-bottom,right-top)
# remember: a parallel is a horizontal specified by latitude angle from the mercator
# returns 1 on success
# returns 0 on failure
sub bounded_by {
	my $self = $_[0];
	my $m = $_[1];

	my $parent = ( caller(1) )[3] || "N/A";
	my $whoami = ( caller(0) )[3];

	my $rm = ref $m;
	if( $rm eq 'ARRAY' and @$m == 4 ){
		# an array ref of 4 items
		# left,bottom,right,top = min-LAT, min-LON, max-LAT, max-LON
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, @$m
	} elsif( $rm eq '' and @_ == 5 ){
		# an array of 4 items (note 4+1=5)
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, $_[1], $_[2], $_[3], $_[4]
	} elsif( $rm eq '' and @_ == 2 ){
		# a string to be parsed as a list of 4 items
		my $spec = Geo::BoundingBox::parse_LATLON_bbox_string($m);
		if( ! defined $spec ){ print STDERR "$whoami (via $parent) : call to ".'Geo::BoundingBox::parse_LATLON_bbox_string()'." has failed for input string '$m'.\n"; return 0 }
		splice @{$self->{'bbox'}}, 0, scalar @{$self->{'bbox'}}, @$spec
	} else {
		print STDERR "$whoami (via $parent) : it accepts [min-LAT,min-LON,max-LAT,max-LON].\n";
		return 0
	}
	return 1 # success
}

# static method to parse a string of (lat,lon,lat,lon)
# representing the bottom-left and top-right of a bbox
# the input is a string of lat[,: ]lon[,: ]lat[,: ]lon[,: ]
# representing the lower-left corner and upper-right corner of a bbox
# using the LAT,LON convention
# returns an arrayref of [lat,lon,lat,lon] on success
# returns undef on error
sub parse_LATLON_bbox_string {
	my $instr = $_[0];

	my $parent = ( caller(1) )[3] || "N/A";
	my $whoami = ( caller(0) )[3];

	if( $instr !~ /^([-+]?([1-8]?\d(\.\d+)?|90(\.0+)?))\s*[\:, ]\s*([-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?))\s*[\:, ]\s*([-+]?([1-8]?\d(\.\d+)?|90(\.0+)?))\s*[\:, ]\s*([-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?))$/ ){ print STDERR "$whoami (via $parent) : input string '$instr' does not match 'lat:lon,lat:lon'. Specify bottom-left and top-right coordinates as LAT,LON using a comma or colon separated list.\n"; return undef }
	my $min_lat = $1;
	my $min_lon = $5;
	my $max_lat = $12;
	my $max_lon = $16;
	if( ! defined $max_lon ){ print STDERR "$whoami (via $parent) : input string '$instr' does not match 'lat:lon,lat:lon'. Specify bottom-left and top-right coordinates as LAT,LON using a comma or colon separated list (2).\n"; return undef }
	return [$min_lat, $min_lon, $max_lat, $max_lon]
}
# static method to parse a string of (lat,lon,width[,height])
# representing the centre and dimensions of a bbox
# the input is a string of lat[,: ]lon[,: ]width[,:x]height
# if height is omitted then the box is a square.
# returns an arrayref of [lat,lon,width,height] on success
# returns undef on error
sub parse_centred_at_string {
	my $instr = $_[0];

	my $parent = ( caller(1) )[3] || "N/A";
	my $whoami = ( caller(0) )[3];

	if( $instr !~ /^([-+]?([1-8]?\d(\.\d+)?|90(\.0+)?))\s*[\:, ]\s*([-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?))\s*[\:, ]\s*([0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)((\s*[,:x]\s*[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)?)$/ ){ print STDERR "$whoami (via $parent) : input string '$instr' does not match 'lat:lon,width[,height]'. Specify a centre point as (LAT,LON) followed by width and optionally by height, width and height in meters (1).\n"; return undef }
	my $lat = $1;
	my $lon = $5;
	my $width = $12;
	if( ! defined $width ){ print STDERR "$whoami (via $parent) : input string '$instr' does not match 'lat:lon,width[,height]'. Specify a centre point as (LAT,LON) followed by width and optionally by height, width and height in meters (2).\n"; return undef }
	my $height = $15;
	if( defined $height ){ $height =~ s/^\s*[,:x]\s*// } else { $height = $width }

	# this can be processed by the centred_at()
	return [$lat,$lon,$width,$height]
}

# returns 1 if self and other are equal within the optional specified precision
# of number of decimal places. Default is metre-precision meaning 5 decimal places
# actually 5 dec places mean 1.1m of accuracy so 6 dec places is probably centimetre
sub equals {
	my $self = $_[0];
	my $other = $_[1];
	my $precision = $_[2]; # optional precision. default is meters (which is 5 decimal places)
	$precision = 5 unless defined $precision;

	my $sb = $self->as_LATLON_bbox();
	my $ob = $other->as_LATLON_bbox();
	return
		Geo::BoundingBox::_numbers_equal_with_precision($sb->[0], $ob->[0], $precision)
	     && Geo::BoundingBox::_numbers_equal_with_precision($sb->[1], $ob->[1], $precision)
	     && Geo::BoundingBox::_numbers_equal_with_precision($sb->[2], $ob->[2], $precision)
	     && Geo::BoundingBox::_numbers_equal_with_precision($sb->[3], $ob->[3], $precision)
}
# returns 1 if self encloses other within the optional specified precision
# (encloses does not me equal, it means just less or more)
# of number of decimal places. Default is metre-precision meaning 5 decimal places
# actually 5 dec places mean 1.1m of accuracy so 6 dec places is probably centimetre
sub encloses {
	my $self = $_[0];
	my $other = $_[1];
	my $precision = $_[2]; # optional precision. default is meters (which is 5 decimal places)
	$precision = 5 unless defined $precision;

	my $sb = $self->as_LATLON_bbox();
	my $ob = $other->as_LATLON_bbox();
	# remember: left,bottom,right,top
	return
		Geo::BoundingBox::_number_less_than_with_precision($sb->[0], $ob->[0], $precision)
	     && Geo::BoundingBox::_number_less_than_with_precision($sb->[1], $ob->[1], $precision)
	     && Geo::BoundingBox::_number_less_than_with_precision($ob->[2], $sb->[2], $precision)
	     && Geo::BoundingBox::_number_less_than_with_precision($ob->[3], $sb->[3], $precision)
}
sub as_LATLON_bbox { return $_[0]->{'bbox'} }
sub as_OSM_bbox {
	my $bb = $_[0]->as_LATLON_bbox();
	return [$bb->[1], $bb->[0], $bb->[3], $bb->[2]]
}
sub stringify_as_OSM_bbox {
	# OSM box is LON,LAT fucking shit
	my $self = $_[0];
	return join(",", @{$self->as_OSM_bbox()})
}
sub stringify_as_LATLON_bbox {
	# LAT,LON but otherwise OSM-like
	my $self = $_[0];
	return join(",", @{$self->as_LATLON_bbox()})
}
sub stringify_as_OSM_bbox_query_xml {
	my $self = $_[0];
	return '<bbox-query'
		. ' w="'.$self->min_lon().'"'
		. ' s="'.$self->min_lat().'"'
		. ' e="'.$self->max_lon().'"'
		. ' n="'.$self->max_lat().'"'
		. '/>'
}
sub min_lon { return $_[0]->{'bbox'}->[1] }
sub min_lat { return $_[0]->{'bbox'}->[0] }
sub max_lon { return $_[0]->{'bbox'}->[3] }
sub max_lat { return $_[0]->{'bbox'}->[2] }

# private methods
# compare 2 floating point numbers with dp (decimal point) precision
# from https://www.oreilly.com/library/view/perl-cookbook/1565922433/ch02s03.html
# BUT!!! : %.3g (as suggested in link) includes integer part, so use %.3f!!!
# sprintf is wrong.
sub _numbers_equal_with_precision {
	my ($A, $B, $dp) = @_;
	# this is wrong because sprintf rounds!!!! we want to truncate
	#return sprintf("%.${dp}f", $A) eq sprintf("%.${dp}f", $B);
	#print "_numbers_equal_with_precision : ($A, $B) => ".($A * 10**$dp).", ".($B * 10**$dp)."\n";
	return int($A * 10**$dp) == int($B * 10**$dp)
}
sub _number_less_than_with_precision {
	my ($A, $B, $dp) = @_;
	#print "_number_less_than_with_precision : ($A, $B) => ".($A * 10**$dp).", ".($B * 10**$dp)."\n";
	return int($A * 10**$dp) < int($B * 10**$dp)
}
# Methods copied (and slightly modified) from Geo::Calc's boundry_box()
# (by Sorin Alexandru Pop) (see https://metacpan.org/pod/Geo::Calc#boundry_box)
# in order to skip all that moose baggage crap
# modifications are in input params only, not in calculations.
sub _boundry_box {
	my (
		$lat, $lon, # centre
		$width, $height # dimensions of rectangular bounding box
	) = @_;

	$width *= 2.0;
	my @points = (
		Geo::BoundingBox::_destination_point($lat, $lon, 0,   $height / 2),
		Geo::BoundingBox::_destination_point($lat, $lon, 90,  $width  / 2),
		Geo::BoundingBox::_destination_point($lat, $lon, 180, $height / 2),
		Geo::BoundingBox::_destination_point($lat, $lon, 270, $width  / 2)
	);
	return [
		# LAT-LON, bounding box, e.g. left,bottom,right,top
		$points[2]->{lat}, # lat_min
		$points[3]->{lon}, # lon_min
		$points[0]->{lat}, # lat_max
		$points[1]->{lon}, # lon_max
	]
}
sub _destination_point {
	my (
		$lat1, $lon1, # centre as LAT,LON
		$brng,
		$s # in meters
	) = @_;

	my $r_major = 6378137;		   # Equatorial Radius, WGS84
	my $r_minor = 6356752.314245179; # defined as constant
	my $f	   = 1/298.257223563;   # 1/f=( $r_major - $r_minor ) / $r_major

	my $alpha1 = Math::Trig::deg2rad( $brng );
	my $sinAlpha1 = sin( $alpha1 );
	my $cosAlpha1 = cos( $alpha1 );

	my $tanU1 = ( 1 - $f ) * tan( Math::Trig::deg2rad( $lat1 ) );

	my $cosU1 = 1 / sqrt( (1 + $tanU1*$tanU1) );
	my $sinU1 = $tanU1 * $cosU1;
	my $sigma1 = atan2( $tanU1, $cosAlpha1 );
	my $sinAlpha = $cosU1 * $sinAlpha1;
	my $cosSqAlpha = 1 - $sinAlpha*$sinAlpha;

	my $uSq = $cosSqAlpha * ( ( $r_major * $r_major ) - ( $r_minor * $r_minor ) ) / ( $r_minor * $r_minor );
	my $A = 1 + $uSq/16384*(4096+$uSq*(-768+$uSq*(320-175*$uSq)));
	my $B = $uSq/1024 * (256+$uSq*(-128+$uSq*(74-47*$uSq)));

	my $sigma = $s / ($r_minor*$A);
	my $sigmaP = pi2;

	my $cos2SigmaM = cos(2*$sigma1 + $sigma);
	my $sinSigma = sin($sigma);
	my $cosSigma = cos($sigma);

	while ( abs($sigma-$sigmaP) > 1e-12 ) {
		$cos2SigmaM = cos(2*$sigma1 + $sigma);
		$sinSigma = sin($sigma);
		$cosSigma = cos($sigma);

		my $deltaSigma = $B*$sinSigma*($cos2SigmaM+$B/4*($cosSigma*(-1+2*$cos2SigmaM*$cos2SigmaM)-
		  $B/6*$cos2SigmaM*(-3+4*$sinSigma*$sinSigma)*(-3+4*$cos2SigmaM*$cos2SigmaM)));
		$sigmaP = $sigma;
		$sigma = $s / ($r_minor*$A) + $deltaSigma;
	}

	my $tmp = $sinU1*$sinSigma - $cosU1*$cosSigma*$cosAlpha1;
	my $lat2 = atan2( $sinU1*$cosSigma + $cosU1*$sinSigma*$cosAlpha1, (1-$f)*sqrt($sinAlpha*$sinAlpha + $tmp*$tmp) );

	my $lambda = atan2($sinSigma*$sinAlpha1, $cosU1*$cosSigma - $sinU1*$sinSigma*$cosAlpha1);
	my $C = $f/16*$cosSqAlpha*(4+$f*(4-3*$cosSqAlpha));
	my $L = $lambda - (1-$C) * $f * $sinAlpha * ($sigma + $C*$sinSigma*($cos2SigmaM+$C*$cosSigma*(-1+2*$cos2SigmaM*$cos2SigmaM)));

	# Normalize longitude so that its in range -PI to +PI
	my $lon2 = POSIX::fmod( Math::Trig::deg2rad( $lon1 ) + $L + ( pi * 3 ), pi2 ) - pi;
	my $revAz = atan2($sinAlpha, -$tmp);  # final bearing, if required

	return {
		lat => Math::Trig::rad2deg($lat2),
		lon => Math::Trig::rad2deg($lon2),
		final_bearing => Math::Trig::rad2deg($revAz)
	};
}
# pod starts here
=encoding utf8
=head1 NAME

Geo::BoundingBox - a bounding box on geographical coordinates

=head1 VERSION

Version 0.01


=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use Geo::BoundingBox;

    my $bbox = Geo::BoundingBox->new();
    # make a square box of 100 meters side,
    # centre the box at 40.596850, 43.083432 (LAT, LON)
    $bbox->centred_at([40.596850, 43.083432, 100]);
    # same
    $bbox->centred_at(40.596850, 43.083432, 100);
    # rectangle 100m x 200m
    $bbox->centred_at(40.596850, 43.083432, 100, 200]);
    # same
    $bbox->centred_at(40.596850, 43.083432, 100, 200);
    # same
    $bbox->centred_at("40.596850:43.083432, 100x200");

    # or create a box specified by bottom-left and top-right coordinates (LAT,LON)
    $bbox->bounded_by(40.595850, 43.082432, 40.597850, 43.084432);

    # overloaded stringify (prints bottom-left, top-right coordinates (LAT,LON))
    print "box created $bbox\n";

    # make a much bigger bbox centred at same location
    my $BBOX = Geo::BoundingBox->new();
    $bbox->centred_at([40.596850, 43.083432, 1000]);

    if( $BBOX->encloses($bbox) ){ print "$BBOX encloses $bbox\n" }
    if( $BBOX->equals($bbox, 3) ){ print "$BBOX equals $bbox to 3 decimal places\n" }


=head1 EXPORT

parse_LATLON_bbox_string

parse_centred_at_string


=head1 SUBROUTINES/METHODS

=head2 C<new()>

Constructor. It takes no parameters.


=head2 C<centred_at(lat,lon,width[,height])>

Build the bounding box by specifying a centre (LAT,LON)
a width and optional height. It expects an array or arrayref
of 3 or 4 items or a string version of those.
First two are (LAT,LON) of the centre,
next is width. Optional fourth is height.
The string version can be comma separated, or have the
coordinate as colon separated followed by a comma, then
width and 'x' followed by height,

It returns 1 on success.
It returns 0 on failure (invalid parameters).

The function to
convert a centred-at specification to a bounding box of two
opposite corners was shamelessly copied with minor modifications affecting
the interface but not the calculations, from L<Geo::Calc>'s boundry_box() (by
Sorin Alexandru Pop). I have decided to do that because I did not want
to carry L<Moose> baggage and include the full L<Geo::Calc>.

=head2 C<centred_at_from_string($str)

Like C<centred_at> but all the parameters 

=head2 C<bounded_by($left_bottom_lat, $left_bottom_lon, $right_top_lat, $right_top_lon)>

Build the bounding box by specifying a the coordinates (as LAT,LON)
of the bottom-left and top-right corners. It expects an array or arrayref
of exactly four items or a string version of those (comma, space or colon separated).

It returns 1 on success.
It returns 0 on failure (invalid parameters).


=head2 C<parse_centred_at_string($str)>

A static exportable function which takes in an input string
containing a centred-at specification: centre-lat:centre-long,width[,height],
parses it and returns an arrayref of its items. Separators can be :x, and space.


=head2 C<parse_bounded_by_string($str)>

A static exportable function which takes in an input string
containing a bounding box: bottom-left-lat:bottom-left-lon,top-right-lat:top-right-lon
parses it and returns an arrayref of its items. Separators can be :, and space.


=head2 C<<equals($obj[,$precision])>>

Compares self with C<$obj> up to C<$precision> decimal places (that's
the number of digits after the funny dot, i.e. it does not include the integer
part). In avoids funny rounding initiatives by the interpreter by multiplying
the numbers by 10 ** $precision and then comparing the integer parts.

It returns 1 if self's and C<$obj>'s corresponding coordinates are equal to
C<$precision> decimal places. Default C<$precision> is 5 decimal places which
amounts to 1.1 meter approximately.


=head2 C<<encloses($obj[,$precision])>>

Checks if self encloses C<$obj> up to C<$precision> decimal places (that's
the number of digits after the funny dot, i.e. it does not include the integer
part). In avoids funny rounding initiatives by the interpreter by multiplying
the numbers by 10 ** $precision and then comparing the integer parts.

The definition of encloses is that ALL coordinates of C<$obj> fall inside the bounding box
of self. Inside does not mean onto. The checks are for less-than and not for less-than-or-equal.

It returns 1 if self encloses C<$obj>. Default C<$precision> is 5 decimal places which
amounts to 1.1 meter approximately.


=head2 C<as_LATLON_bbox()>

Return a reference to the internal bounding-box array (and not a new array).
The internal bounding-box array is C<[bottom-left-lat,bottom-left-lon,top-right-lat,top-right-lon]>


=head2 C<as_OSM_bbox()>

Returns an arrayref of 4 items representing a bounding box following OpenStreetMap (OSM) convention
which puts LON first and LAT second: C<[bottom-left-lon,bottom-left-lat,top-right-lon,top-right-lat]>


=head2 C<stringify_as_OSM_bbox()>

It returns a string representation (comma separated)
of the array returned by C<as_OSM_bbox()>.


=head2 C<stringify_as_LATLON_bbox()>

It returns a string representation (comma separated)
of the array returned by C<as_LATLON_bbox()>.


=head2 C<stringify_as_OSM_bbox_query_xml()>

It returns a string to be used as a C<bbox-query>
for the OpenStreetMap (OSM) Overpass API. This is
an XML string tagged "bbox-query" with for items

=over 4
=item C<w> is the longitude of the bottom-left corner
=item C<s> is the latitude of the bottom-left corner
=item C<e> is the longitude of the top-right corner
=item C<n> is the latitude of the top-right corner
=back

head C<min_lon()>

It returns the longitude of the bottom-left corner.

head C<min_lat()>

It returns the latitude of the bottom-left corner.

head C<max_lon()>

It returns the longitude of the top-right corner.

head C<max_lat()>

It returns the latitude of the top-right corner.


=head1 AUTHOR

Andreas Hadjiprocopis, C<< <bliako at cpan.org> >>

The original author of the function (C<Geo::BoundingBox::_boundry_box>) to convert a centred-at specification
to a bounding box is Sorin Alexandru Pop (see https://metacpan.org/pod/Geo::Calc)


=head1 BUGS

Please report any bugs or feature requests to C<bug-geo-bbox at rt.cpan.org>, or through
the web interface at L<https://rt.cpan.org/NoAuth/ReportBug.html?Queue=Geo-BoundingBox>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Geo::BoundingBox


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<https://rt.cpan.org/NoAuth/Bugs.html?Dist=Geo-BoundingBox>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Geo-BoundingBox>

=item * CPAN Ratings

L<https://cpanratings.perl.org/d/Geo-BoundingBox>

=item * Search CPAN

L<https://metacpan.org/release/Geo-BoundingBox>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2019 Andreas Hadjiprocopis.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1; # End of Geo::BoundingBox
