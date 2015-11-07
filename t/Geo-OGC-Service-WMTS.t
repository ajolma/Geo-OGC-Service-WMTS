# Before 'make install' is performed this script should be runnable with
# 'make test'. After 'make install' it should work as 'perl Geo-OGC-Service.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use warnings;

use Test::More tests => 1;
use Plack::Test;
use HTTP::Request::Common;
use Geo::OGC::Service;
use XML::LibXML;
use XML::SemanticDiff;
BEGIN { use_ok('Geo::OGC::Service::WMTS') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

