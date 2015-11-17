=pod

=head1 NAME

Geo::OGC::Service::WMTS - Perl extension to create geospatial web map tile services

=head1 SYNOPSIS

The process_request method of this module is called by the
Geo::OGC::Service framework. 

In a psgi script write something like

 use Geo::OGC::Service::WMTS;

 my $ogc = Geo::OGC::Service->new({
 config => '/var/www/etc/OGC-services.conf',
 services => {
        'WFS' => 'Geo::OGC::Service::WFS',
        'WMTS' => 'Geo::OGC::Service::WMTS',
        'WMS' => 'Geo::OGC::Service::WMTS',
        'TMS' => 'Geo::OGC::Service::WMTS',
 }});

 builder {
    mount "/WFS" => $ogc->to_app;
    mount "/WMTS" => $ogc->to_app;
    mount "/TMS" => $ogc->to_app;
    mount "/" => $default;
 };

=head1 DESCRIPTION

This module aims to provide the operations defined by the Open
Geospatial Consortium's Web Map Tile Service standard.

This module is a plugin for the Geo::OGC::Service framework.

Additionally, this module aims to support WMS used as WMTS and TMS.

=head1 CONFIGURATION

The configuration is defined similarly as to other services under
Geo::OGC::Service, either as a file or as a variable in the call to
Geo::OGC::Service->new.

The file must be JSON and either have top level key WMTS, WMS, or TMS
if more than one service is defined. The value of the key must be a
hash, or the name of a key, which has a hash value.

Known top level keys in the hash are 'resource', 'blank', 'debug', and
'TileSets'. TileSets is an array of hashes. The keys of a TileSet hash
are Layers, Format, Resolutions, SRS, BoundingBox, path, and ext.

=head2 EXPORT

None by default.

=head2 METHODS

=cut

package Geo::OGC::Service::WMTS;

use 5.010000; # say // and //=
use feature "switch";
use Carp;
use File::Basename;
use Modern::Perl;
use JSON;
use Geo::GDAL;
use Cwd;
use Math::Trig;
use HTTP::Date;

use Data::Dumper;
use XML::LibXML::PrettyPrint;

use Geo::OGC::Service;
use vars qw(@ISA);
push @ISA, qw(Geo::OGC::Service::Common);

our $VERSION = '0.01';

our $radius_of_earth_at_equator = 6378137;
our $standard_pixel_size = 0.28 / 1000;
our $tile_width = 256;
our $tile_height = 256;
our $originShift3857 = pi * $radius_of_earth_at_equator;

our %projections = (
    'EPSG:3857' => {
        identifier => 'EPSG:3857',
        crs => 'urn:ogc:def:crs:EPSG:6.3:3857',
        extent => {
            SRS => 'EPSG:3857', 
            minx => -1 * $originShift3857,
            miny => -1 * $originShift3857,
            maxx => $originShift3857,
            maxy => $originShift3857  },
    },
    'EPSG:3067' => {
        identifier => 'ETRS-TM35FIN',
        crs => 'urn:ogc:def:crs:EPSG:6.3:3067',
        extent => { 
            SRS => 'EPSG:3067', 
            # JHS180 liite 1:
            minx => -548576,
            miny => 6291456,
            maxx => 1548576,
            maxy => 8388608 }
    }
);

=pod

=head3 process_request

The entry method into this service. Calls RESTful if there is no
request parameter, otherwise dispatches the call to
WMSGetCapabilities, GetCapabilities, GetTile, GetMap, or FeatureInfo
depending on service and request. If request is not recognized,
returns an error XML with exceptionCode => 'InvalidParameterValue'.

=cut

sub process_request {
    my ($self, $responder) = @_;
    $self->{debug} = $self->{config}{debug};
    if ($self->{debug}) {
        if ($self->{debug} > 2) {
            $self->log($self);
        } elsif ($self->{debug} > 1) {
            $self->log({ service => $self->{service},
                         parameters => $self->{parameters},
                         request => $self->{request} });
        } else {
            $self->log({ service => $self->{service},
                         parameters => $self->{parameters} });
        }
    }
    $self->{responder} = $responder;
    $self->{request} = $self->{parameters}{request} // '';
    my $response;
    for ($self->{request} // '') {
        if ($self->{service} eq 'WMS' and (/^GetCapabilities/ or /^capabilities/)) { $self->WMSGetCapabilities() }
        elsif (/^GetCapabilities/ or /^capabilities/) { $self->GetCapabilities() }
        elsif (/^GetTile/)                         { $response = $self->GetTile() }
        elsif (/^GetMap/)                          { $response = $self->GetMap() }
        elsif (/^FeatureInfo/)                     { $response = $self->FeatureInfo() }
        elsif (/^$/)                               { $response = $self->RESTful() }
        else                                       { 
            $self->error({ exceptionCode => 'InvalidParameterValue',
                           locator => 'request',
                           ExceptionText => "$self->{parameters}{request} is not a known request" }) }
    }
    $self->{responder}->($response) if $response;
}

=pod

=head3 GetCapabilities

Sends a capabilities document according to WMTS standard.

=cut

sub GetCapabilities {
    my ($self) = @_;
    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();
    $writer->open_element(Capabilities => { 
        version => '1.0.0',
        xmlns => "http://www.opengis.net/wmts/1.0",
        'xmlns:ows' => "http://www.opengis.net/ows/1.1",
        'xmlns:xlink' => "http://www.w3.org/1999/xlink",
        'xmlns:xsi' => "http://www.w3.org/2001/XMLSchema-instance",
        'xmlns:gml' => "http://www.opengis.net/gml",
        'xsi:schemaLocation' => "http://www.opengis.net/wmts/1.0 ".
            "http://schemas.opengis.net/wmts/1.0/wmtsGetCapabilities_response.xsd",
    });
    $self->DescribeService($writer);
    $writer->open_element('ows:OperationsMetadata');
    for my $operation (qw/GetCapabilities GetTile GetFeatureInfo/) {
        $self->Operation( $writer, $operation, { Get => [ 'ows:AllowedValues' => ['ows:Value' => 'KVP' ] ] } );
    }
    $writer->close_element;
    $writer->open_element(Contents => {});

    my $t_srs = Geo::OSR::SpatialReference->new(EPSG => 4326);

    for my $set (@{$self->{config}{TileSets}}) {
        my $projection = $projections{$set->{SRS}};

        my $bb;
        if ($set->{BoundingBox}) {
            my ($epsg) = $set->{BoundingBox}{SRS} =~ /(\d+)/;
            my $s_srs = Geo::OSR::SpatialReference->new(EPSG => $epsg);
            my $ct = Geo::OSR::CoordinateTransformation->new($s_srs, $t_srs);

            my $x = $set->{BoundingBox};
            $x = $projection->{extent};

            my $points = [[$x->{minx}, $x->{miny}],
                          [$x->{maxx}, $x->{maxy}]];

            $ct->TransformPoints($points);

            $bb = [ 'ows:WGS84BoundingBox' => { crs => "urn:ogc:def:crs:OGC:2:84" },
                   [ [ 'ows:LowerCorner' => "$points->[0][0] $points->[0][1]" ],
                     [ 'ows:UpperCorner' => "$points->[1][0] $points->[1][1]" ] ] ];
            
        }

        my ($ext) = $set->{Format} =~ /(\w+)$/;
        my @layer = (
            [ 'ows:Title' => $set->{Title} // $set->{Layers} ],
            [ 'ows:Identifier' => $set->{Layers} ],
            [ 'Style' => { isDefault => 'true' }, [ 'ows:Identifier' => 'default' ] ],
            [ Format => $set->{Format} ],
            [ TileMatrixSetLink => [ TileMatrixSet => $projection->{identifier} ] ],
            [ ResourceURL => { 
                resourceType => 'tile',
                format => $set->{Format},
                template => "$self->{config}{resource}/$set->{Layers}/{TileMatrix}/{TileCol}/{TileRow}.$ext"
              }]
        );
        push @layer, $bb if $bb;
        $writer->element('Layer' => \@layer );
    };
    for my $projection (keys %projections) {
        tile_matrix_set($writer, $projections{$projection}, [0..17]); # GDAL uses the highest value
    }
    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

=pod

=head3 WMSGetCapabilities

Sends a capabilities document according to WMS standard.

=cut

sub WMSGetCapabilities {
    my ($self) = @_;

    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();

    $writer->open_element(WMT_MS_Capabilities => { version => '1.1.1' });
    $writer->element(Service => [
                         [Name => 'OGC:WMS'],
                         ['Title'],
                         [OnlineResource => {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                                             'xlink:href' => $self->{config}{resource}}]]);
    $writer->open_element('Capability');
    $writer->element(Request => 
                     [[GetCapabilities => 
                       [[Format => 'application/vnd.ogc.wms_xml'],
                        [DCPType => 
                         [HTTP => 
                          [Get => 
                           [OnlineResource => 
                            {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                             'xlink:href' => $self->{config}{resource}}]]]]]],
                      [GetMap => 
                       [[Format => 'image/png'],
                        [DCPType => 
                         [HTTP => 
                          [Get => 
                           [OnlineResource => 
                            {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                             'xlink:href' => $self->{config}{resource}}]]]]]]
                     ]);
    $writer->element(Exception => [Format => 'text/plain']);
    
    for my $set (@{$self->{config}{TileSets}}) {
        my($i0,$i1) = split /\.\./, $set->{Resolutions};

        #my @resolutions = @resolutions_3857[$i0..$i1]; # with this QGIS starts to ask higher resolution tiles
        my @resolutions;
        my $projection = $projections{$set->{SRS}};
        my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
        for my $i (0..19) {
            $resolutions[$i] = $extent_width/(2**$i * $tile_width);
        }

        my $bb = $set->{BoundingBox}; # with this QGIS does not show tiles at correct locations
        $bb = $projection->{extent};

        $writer->element(VendorSpecificCapabilities => 
                         [TileSet => [[SRS => $set->{SRS}],
                                      [BoundingBox => $bb],
                                      [Resolutions => "@resolutions"],
                                      [Width => $set->{Width} // $tile_width],
                                      [Height => $set->{Height} // $tile_height],
                                      [Format => $set->{Format}],
                                      [Layers => $set->{Layers}],
                                      [Styles => undef]]]);
    }

    $writer->element(UserDefinedSymbolization => 
                     {SupportSLD => 0, UserLayer => 0, UserStyle => 0, RemoteWFS => 0});

    for my $set (@{$self->{config}{TileSets}}) {

        my $projection = $projections{$set->{SRS}};

        my $bb = $set->{BoundingBox}; # with this QGIS does not show tiles at correct locations
        $bb = $projection->{extent};

        $writer->element(Layer => [[Title => 'TileCache Layers'],
                                   [Layer => {queryable => 0, opaque => 0, cascaded => 1}, 
                                    [[Name => $set->{Layers}],
                                     [Title => $set->{Layers}],
                                     [SRS => $set->{SRS}],
                                     [Format => $set->{Format}],
                                     [BoundingBox => $bb]
                                    ]]
                         ]);
    }

    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

=pod

=head3 GetMap

Serves the tile request if WMS is used.

Sends the requested tile based on parameters BBOX and LAYERS.

The tiles should be in a tile map resource type of directory structure
(z/y/x.png). The value of the 'path' key in the TileSet config element
should point to the directory.

=cut

sub GetMap {
    my ($self) = @_;
    for my $param (qw/bbox layers/) {
        unless ($self->{parameters}{$param}) {
            $self->error({ exceptionCode => 'MissingParameterValue',
                           locator => uc($param) });
            return;
        }
    }
    my $set;
    for my $s (@{$self->{config}{TileSets}}) {
        if ($s->{Layers} eq $self->{parameters}{layers}) {
            $set = $s;
            last;
        }
    }
    unless ($set) {
        $self->error({ exceptionCode => 'InvalidParameterValue',
                       locator => 'LAYERS' });
        return;
    }

    my $projection = $projections{$set->{SRS}};
    my @resolutions;
    my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
    for my $i (0..19) {
        $resolutions[$i] = $extent_width/(2**$i * $tile_width);
    }
    
    my @bbox = split /,/, $self->{parameters}{bbox}; # minx, miny, maxx, maxy
    my $units_per_pixel = ($bbox[2]-$bbox[0])/$tile_width;
    my $matrix;
    my $res;
    my $i = 0;
    for my $r (@resolutions) {
        if (abs($r - $units_per_pixel) < 0.1) {
            $res = $r;
            $matrix = $i;
            last;
        }
        $i++;
    }
    my $rows = 2**$matrix;

    # from globalmaptiles.py by Klokan Petr Pridal:

    my $originShift = 2 * pi * 6378137 / 2.0;

    my $px = ($bbox[0] + $originShift) / $res;
    my $py = ($bbox[1] + $originShift) / $res;

    my $col = int( POSIX::ceil( $px / $tile_width ) - 1 );
    my $row = int( POSIX::ceil( $py / $tile_height ) - 1 );

    ($set->{ext}) = $set->{Format} =~ /(\w+)$/;

    if ($set->{file}) {
        $self->{parameters}{tilematrix} = $matrix;
        $self->{parameters}{tilecol} = $row;
        $self->{parameters}{tilerow} = 2**$matrix-($col+1);
        return $self->make_tile($set);
    }

    return $self->tile("$set->{path}/$matrix/$col/$row.$set->{ext}", $set->{Format});
}

=pod

=head3 GetTile

Serves the tile request if WMTS is used.

Sends the requested tile based on parameters Layer, Tilerow, Tilecol,
Tilematrix, Tilematrixset, and Format.

The tile is served from a tile map directory or it is made on the fly
from a GDAL data source (the value of the 'file' key in the TileSet).
In addition, processing may be applied to the data source (the
'processing' key). The processing may be one of those implemented in
GDAL.

Using the 'file' keyword requires GDAL 2.1.

=cut

sub GetTile {
    my ($self) = @_;
    for my $param (qw/layer tilerow tilecol tilematrix tilematrixset format/) {
        return $self->error({ exceptionCode => 'MissingParameterValue',
                              locator => $param }) unless $self->{parameters}{$param};
    }
    ($self->{parameters}{ext}) = $self->{parameters}{format} =~ /(\w+)$/;
    my $set;
    for my $s (@{$self->{config}{TileSets}}) {
        if ($s->{Layers} eq $self->{parameters}{layer}) {
            $set = $s;
            last;
        }
    }
    return $self->error({ exceptionCode => 'InvalidParameterValue',
                          locator => 'layer' }) unless defined $set;

    ($set->{ext}) = $set->{Format} =~ /(\w+)$/;

    return $self->make_tile($set) if $set->{file};
    
    my $matrix = $self->{parameters}{tilematrix};
    my $col = $self->{parameters}{tilecol};
    my $row = 2**$matrix-$self->{parameters}{tilerow}-1;
    my $ext = $self->{parameters}{ext} // $set->{ext};
    return $self->tile("$set->{path}/$matrix/$col/$row.$ext", $set->{Format});

}

=pod

=head3 RESTful

RESTful service. The URL should have the form
<service>/layer/<TileMatrixSet>/<TileMatrix>/<TileCol>/<TileRow>.<ext>.
TileMatrixSet is optional. Compare this to the template in
capabilities.

Sends TileMapService response if the layer is not in the URL, TileMap
response if the layer is in the URL but zoom, row, and col are not, or
the requested tile based on layer, zoom, row, and column in the URL.

=cut

sub RESTful {
    my ($self) = @_;
    my $path = $self->{env}{PATH_INFO};
    #print STDERR "$path\n";
    my ($layer) = $path =~ /^\/(\w+)/;
    return $self->tilemaps unless defined $layer;
    my $set;
    for my $s (@{$self->{config}{TileSets}}) {
        $set = $s, last if $s->{Layers} eq $layer;
    }
    return $self->error({ exceptionCode => 'InvalidParameterValue',
                          locator => 'layer' }) unless defined $set;
    $path =~ s/^\/(\w+)//;
    my ($matrix, $col, $row, $ext) = $path =~ /^\/(\w+)\/(\w+)\/(\w+)\.(\w+)$/;
    unless (defined $matrix) {
        ($self->{parameters}{tilematrixset}, $matrix, $col, $row, $ext) = 
            $path =~ /^\/([\w\:]+)\/(\w+)\/(\w+)\/(\w+)\.(\w+)$/;
    }
    return $self->tilemapresource($set) unless defined $matrix;

    if ($set->{file}) {
        $self->{parameters}{ext} = $ext;
        $self->{parameters}{format} = "image/$ext";
        $self->{parameters}{layer} = $layer;
        $self->{parameters}{tilematrix} = $matrix;
        $self->{parameters}{tilecol} = $col;
        $self->{parameters}{tilerow} = 2**$matrix-($row+1);
        return $self->make_tile($set);
    }

    #gdal:
    $row = 2**$matrix-($row+1);

    return $self->tile("$set->{path}/$matrix/$col/$row.$set->{ext}", $set->{Format});
}

=pod

=head3 FeatureInfo

Not yet implemented.

=cut

sub FeatureInfo {
    my ($self) = @_;
    return error_403();
}

sub make_tile {
    my ($self, $set) = @_;

    $self->log($self->{parameters});

    return $self->error({ exceptionCode => 'ResourceNotFound',
                          ExceptionText => "File resources are not supported by this GDAL version." })
        unless Geo::GDAL::Dataset->can('Translate');
        
    my $ds = Geo::GDAL::Open($set->{file});

    if (0) {
        my $srs_s = $ds->SpatialReference;
        
        my ($epsg_t) = $set->{SRS} =~ /(\d+)/;
        my $srs_t = Geo::OSR::SpatialReference->new(EPSG => $epsg_t);
        
        if (!$srs_s->IsSame($srs_t)) {
            $ds = $ds->Warp('/vsimem/w.png', );
        }
    }

    my $projection = $projections{$set->{SRS}};
        
    my $tile = Tile->new($projection->{extent}, $self->{parameters});

    eval {
   
        if ($set->{processing}) {
            $tile->expand(1);
            $ds = $ds->Translate( "/vsimem/tmp.png", ['-of' => 'PNG', '-r' => 'bilinear' , 
                                                      '-outsize' , $tile->tile,
                                                      '-projwin', $tile->extent] );
            $ds = $ds->DEMProcessing("/vsimem/tmp2.png", $set->{processing}, undef, { of => 'PNG' });
            $tile->expand(-1);
        }
        
        my $writer = $self->{responder}->([200, [ 'Content-Type' => "image/png" ]]);
            
        $ds->Translate($writer, ['-of' => 'PNG', '-r' => 'bilinear' , 
                                 '-outsize' , $tile->tile,
                                 '-projwin', $tile->extent,
                                 '-projwin_srs', $set->{SRS},
                                 '-a_srs', $set->{SRS}
                       ]);
    };
        
    if ($@) {
        my $err = Geo::GDAL::error();
        say STDERR $err;
        return $self->error({ exceptionCode => 'ResourceNotFound',
                              ExceptionText => $err });
    }
        
    return undef;
}

sub tile {
    my ($self, $tile, $content_type) = @_;
    #print STDERR "tile: $tile, $content_type\n";
    $tile = $self->{config}{blank} unless -r $tile;
    open my $fh, "<:raw", $tile or return error_403();
    my @stat = stat $tile;
    Plack::Util::set_io_path($fh, Cwd::realpath($tile));
    return [ 200, [
                 'Content-Type'   => $content_type,
                 'Content-Length' => $stat[7],
                 'Last-Modified'  => HTTP::Date::time2str( $stat[9] )
             ],
             $fh,
        ];
}

sub tile_matrix_set {
    my ($writer, $projection, $tile_matrix_set) = @_;
    $writer->open_element('TileMatrixSet');
    $writer->element('ows:Identifier' => $projection->{identifier});
    $writer->element(BoundingBox => { crs =>  $projection->{crs} },
                     [ ['ows:LowerCorner' => $projection->{extent}{minx}.' '.$projection->{extent}{miny} ],
                       ['ows:UpperCorner' => $projection->{extent}{maxx}.' '.$projection->{extent}{maxy} ] ]);
    $writer->element('ows:SupportedCRS' => { crs =>  $projection->{crs} }, $projection->{crs});
    my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
    for my $tile_matrix (@$tile_matrix_set) {
        my $matrix_width = 2**$tile_matrix;
        my $matrix_height = 2**$tile_matrix;
        $writer->element(TileMatrix => 
                         [ [ 'ows:Identifier' => $tile_matrix ],
                           [ ScaleDenominator => 
                             $extent_width / 
                             ($matrix_width * $tile_width) / 
                             $standard_pixel_size ],
                           [ TopLeftCorner => $projection->{extent}{minx}.' '.$projection->{extent}{maxy} ],
                           [ TileWidth => $tile_width ],
                           [ TileHeight => $tile_height ],
                           [ MatrixWidth => $matrix_width ],
                           [ MatrixHeight => $matrix_height ] ]);
    }
    $writer->close_element();
}

sub tilemaps {
    my ($self) = @_;
    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();
    $writer->open_element(TileMapService => { version => "1.0.0", 
                                              tilemapservice => "http://tms.osgeo.org/1.0.0" });
    $writer->open_element(TileMaps => {});
    for my $set (@{$self->{config}{TileSets}}) {
        $writer->element(TileMap => {href => $self->{config}{resource}.'/'.$set->{Layers}, 
                                     srs => $set->{SRS}, 
                                     title => $set->{Title}, 
                                     profile => 'none'});
    }
    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
    return undef;
}

sub tilemapresource {
    my ($self, $set) = @_;
    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();
    $writer->open_element(TileMap => { version => "1.0.0", 
                                       tilemapservice => "http://tms.osgeo.org/1.0.0" });
    $writer->element(Title => $set->{Title} // $set->{Layers});
    $writer->element(Abstract => $set->{Abstract} // '');
    $writer->element(SRS => $set->{SRS} // 'EPSG:3857');
    $writer->element(BoundingBox => $set->{BoundingBox});
    $writer->element(Origin => {x => $set->{BoundingBox}{minx}, y => $set->{BoundingBox}{miny}});
    my ($ext) = $set->{Format} =~ /(\w+)$/;
    $writer->element(TileFormat => { width => $set->{Width} // $tile_width, 
                                     height => $set->{Height} // $tile_height, 
                                     'mime-type' => $set->{Format}, 
                                     extension => $ext });
    my @sets;
    my ($n, $m) = $set->{Resolutions} =~ /(\d+)\.\.(\d+)$/;
    my $projection = $projections{$set->{SRS}};
    my @resolutions;
    my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
    for my $i (0..19) {
        $resolutions[$i] = $extent_width/(2**$i * $tile_width);
    }
    for my $i ($n..$m) {
        push @sets, [TileSet => {href=>$i, order=>$i, 'units-per-pixel'=>$resolutions[$i]}];
    }
    $writer->element(TileSets => {profile => "mercator"}, \@sets);
    $writer->close_element;
    $writer->stream($self->{responder});
    return undef;
}

sub error {
    my ($self, $msg) = @_;
    if (!$msg->{debug}) {
        Geo::OGC::Service::error($self->{responder}, $msg);
        return undef;
    } else {
        my $json = JSON->new;
        $json->allow_blessed([1]);
        my $writer = $self->{responder}->([200, [ 'Content-Type' => 'application/json',
                                                  'Content-Encoding' => 'UTF-8' ]]);
        $writer->write($json->encode($msg->{debug}));
        $writer->close;
    }
}

sub log {
    my ($self, $msg) = @_;
    say STDERR Dumper($msg);
}

{
    package Tile;
    sub new {
        my ($class, $extent, $parameters) = @_;
        my $self = []; # tile_width tile_height minx maxy maxx miny pixel_width pixel_height
        $self->[0] = $Geo::OGC::Service::WMTS::tile_width;
        $self->[1] = $Geo::OGC::Service::WMTS::tile_height;
        my $extent_width = $extent->{maxx} - $extent->{minx};
        my $extent_height = $extent->{maxy} - $extent->{miny};
        my $matrix_width = 2**$parameters->{tilematrix};
        my $width = $extent_width/$matrix_width;
        my $height = $extent_height/$matrix_width;
        $self->[2] = $extent->{minx} + $parameters->{tilecol} * $width;
        $self->[3] = $extent->{maxy} - $parameters->{tilerow} * $height;
        $self->[4] = $extent->{minx} + ($parameters->{tilecol}+1) * $width;
        $self->[5] = $extent->{maxy} - ($parameters->{tilerow}+1) * $height;
        $self->[6] = $width / $self->[0];
        $self->[7] = $height / $self->[1];
        bless $self, $class;
    }
    sub tile {
        my ($self) = @_;
        return @{$self}[0..1];
    }
    sub extent {
        my ($self) = @_;
        return @{$self}[2..5];
    }
    sub expand {
        my ($self, $pixels) = @_;
        $self->[0] += 2*$pixels;
        $self->[1] += 2*$pixels;
        $self->[2] -= $self->[6]*$pixels;
        $self->[3] += $self->[7]*$pixels;
        $self->[4] += $self->[6]*$pixels; 
        $self->[5] -= $self->[7]*$pixels;
    }
}

sub error_403 {
    [403, ['Content-Type' => 'text/plain', 'Content-Length' => 9], ['forbidden']];
}

1;
__END__

=head1 LIMITATIONS

Currently only EPSG 3067 (ETRS-TM35FIN) and 3857 (Google Mercator) are
supported. To support other tile matrix sets add them to
%Geo::OGC::Service::WMTS::projections.

=head1 SEE ALSO

Discuss this module on the Geo-perl email list.

L<https://list.hut.fi/mailman/listinfo/geo-perl>

For the WMTS standard see 

L<http://www.opengeospatial.org/standards/wmts>

=head1 REPOSITORY

L<https://github.com/ajolma/Geo-OGC-Service-WMTS>

=head1 AUTHOR

Ari Jolma, E<lt>ari.jolma at gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2015 by Ari Jolma

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.22.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
