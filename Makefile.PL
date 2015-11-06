use 5.022000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Geo::OGC::Service::WMTS',
    VERSION_FROM      => 'lib/Geo/OGC/Service/WMTS.pm', # finds $VERSION, requires EU::MM from perl >= 5.5
    PREREQ_PM         => { "Modern::Perl" => 0,
                           "Geo::OGC::Service" => 0.05,
    },
    TEST_REQUIRES     => { "Plack::Test" => 0,
                           "XML::LibXML" => 0,
                           "XML::SemanticDiff" => 0 },
    ABSTRACT_FROM     => 'lib/Geo/OGC/Service/WMTS.pm', # retrieve abstract from module
    AUTHOR            => 'Ari Jolma <ari.jolma at gmail.com>',
    LICENSE           => 'perl'
);
