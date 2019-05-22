use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    INSTALL_BASE     => $ENV{'HOME'}."/usr",
    NAME             => 'Geo::BoundingBox',
    AUTHOR           => q{Andreas Hadjiprocopis <bliako@cpan.org>},
    VERSION_FROM     => 'lib/Geo/BoundingBox.pm',
    ABSTRACT_FROM    => 'lib/Geo/BoundingBox.pm',
    LICENSE          => 'artistic_2',
    PL_FILES         => {},
    MIN_PERL_VERSION => '5.006',
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => '0',
    },
    BUILD_REQUIRES => {
        'Test::More' => '0',
	'Math::Trig' => '1.23',
    },
    PREREQ_PM => {
        #'ABC'              => '1.6',
        #'Foo::Bar::Module' => '5.0401',
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Geo-BoundingBox-*' },
);
