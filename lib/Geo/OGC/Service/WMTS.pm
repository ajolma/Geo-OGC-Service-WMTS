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
use Capture::Tiny ':all';
use Clone 'clone';
use JSON;
use DBI;
use Geo::GDAL;
use Cwd;
use Math::Trig;

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

The entry method into this service. Falls back to TMS is service is
TMS.

=cut

sub process_request {
    my ($self, $responder) = @_;
    $self->{request} = $self->{parameters}{request};
    $self->{debug} = $self->{config}{debug};
    $self->{responder} = $responder;
    if ($self->{parameters}{debug}) {
        $self->error({ 
            debug => { 
                config => $self->{config}, 
                parameters => $self->{parameters}, 
                env => $self->{env},
                request => $self->{request} 
            } });
        return;
    }
    if ($self->{debug}) {
        if ($self->{debug} > 2) {
            $self->log($self);
        } else {
            $self->log($self->{parameters});
        }
    }
    my $response;
    for ($self->{request} // '') {
        if ($self->{service} eq 'TMS' and (/^GetCapabilities/ or /^capabilities/)) { $self->WMSGetCapabilities() }
        elsif (/^GetCapabilities/ or /^capabilities/) { $self->GetCapabilities() }
        elsif (/^GetTile/)                         { $response = $self->GetTile() }
        elsif (/^GetMap/)                          { $response = $self->GetMap() }
        elsif ($self->{service} eq 'TMS')          { $response = $self->TMS() }
        elsif (/^FeatureInfo/)                     { $self->FeatureInfo() }
        elsif (/^$/)                               { 
            $self->error({ exceptionCode => 'MissingParameterValue',
                           locator => 'request' }) }
        else                                       { 
            $self->error({ exceptionCode => 'InvalidParameterValue',
                           locator => 'request',
                           ExceptionText => "$self->{parameters}{request} is not a known request" }) }
    }
    $self->{responder}->($response) if $response;
}

=pod

=head3 GetCapabilities

Sends the capabilities document according to WMTS standard if the
requested service is WMTS and according to WMS if the requested
service is WMS.

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
    for my $set (@{$self->{config}{TileSets}}) {
        my $projection = $projections{$set->{SRS}};
        $writer->element('Layer' => [
                             [ 'ows:Title' => $set->{Title} // $set->{Layers} ],
                             [ 'ows:Identifier' => $set->{Layers} ],
                             [ 'Style' => { isDefault => 'true' }, [ 'ows:Identifier' => 'default' ] ],
                             [ Format => $set->{Format} ],
                             [ TileMatrixSetLink => [ TileMatrixSet => $projection->{identifier} ] ],
                             [ ResourceURL => { 
                                 resourceType => 'tile',
                                 'format' => $set->{Format},
                                 template => $self->{config}{resource} }]
                         ]
            )
    };
    for my $projection (keys %projections) {
        tile_matrix_set($writer, $projections{$projection}, [0..15]);
    }
    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

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
        #my @resolutions = @resolutions_3857;
        my @resolutions;
        my $projection = $projections{$set->{SRS}};
        my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
        for my $i (0..19) {
            $resolutions[$i] = $extent_width/(2**$i * $tile_width);
        }
        $writer->element(VendorSpecificCapabilities => 
                         [TileSet => [[SRS => $set->{SRS}],
                                      #[BoundingBox => $set->{BoundingBox}], # with this QGIS does not show tiles at correct locations
                                      [BoundingBox => $projection->{extent}],
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
        $writer->element(Layer => [[Title => 'TileCache Layers'],
                                   [Layer => {queryable => 0, opaque => 0, cascaded => 1}, 
                                    [[Name => $set->{Layers}],
                                     [Title => $set->{Layers}],
                                     [SRS => $set->{SRS}],
                                     [Format => $set->{Format}],
                                     [BoundingBox => $projection->{extent}],
                                     #[BoundingBox => $set->{BoundingBox}] # with this QGIS does not show tiles at correct locations
                                    ]]
                         ]);
    }

    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

=pod

=head3 GetMap

Sends the requested tile based on parameters BBOX and LAYERS.

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
    my $z;
    my $res;
    my $i = 0;
    for my $r (@resolutions) {
        if (abs($r - $units_per_pixel) < 0.1) {
            $res = $r;
            $z = $i;
            last;
        }
        $i++;
    }
    my $rows = 2**$z;

    # from globalmaptiles.py by Klokan Petr Pridal:

    my $originShift = 2 * pi * 6378137 / 2.0;

    my $px = ($bbox[0] + $originShift) / $res;
    my $py = ($bbox[1] + $originShift) / $res;

    my $tx = int( POSIX::ceil( $px / $tile_width ) - 1 );
    my $ty = int( POSIX::ceil( $py / $tile_height ) - 1 );

    my $file = "$set->{path}/$z/$tx/$ty.$set->{ext}";

    $file = $self->{config}{blank} unless -r $file;

    open my $fh, "<:raw", $file or return $self->return_403;

    my @stat = stat $file;

    Plack::Util::set_io_path($fh, Cwd::realpath($file));
    
    return [ 200, 
             [
              'Content-Type'   => $set->{Format},
              'Content-Length' => $stat[7],
              'Last-Modified'  => HTTP::Date::time2str( $stat[9] )
             ],
             $fh,
        ];
}

=pod

=head3 GetMap

Sends the requested tile based on parameters Layer, Tilerow, Tilecol,
Tilematrix, Tilematrixset, and Format.

=cut

sub GetTile {
    my ($self) = @_;
    for my $param (qw/layer tilerow tilecol tilematrix tilematrixset format/) {
        unless ($self->{parameters}{$param}) {
            $self->error({ exceptionCode => 'MissingParameterValue',
                           locator => $param });
            return;
        }
    }
    my $set;
    for my $s (@{$self->{config}{TileSets}}) {
        if ($s->{Layers} eq $self->{parameters}{layer}) {
            $set = $s;
            last;
        }
    }
    unless ($set) {
        $self->error({ exceptionCode => 'InvalidParameterValue',
                       locator => 'layer' });
        return;
    }

    my $file;

    if ($set->{file}) {

        # clip from file, Translate needs 2.1
        my $ds = Geo::GDAL::Open($set->{file});
        my $projection = $projections{$set->{SRS}};
        my $extent_width = $projection->{extent}{maxx} - $projection->{extent}{minx};
        my $extent_height = $projection->{extent}{maxy} - $projection->{extent}{miny};
        my $matrix_width = 2**$self->{parameters}{tilematrix};
        my $width = $extent_width/$matrix_width;
        my $height = $extent_height/$matrix_width;
        my $minx = $projection->{extent}{minx} + $self->{parameters}{tilecol} * $width;
        my $maxy = $projection->{extent}{maxy} - $self->{parameters}{tilerow} * $height;
        my $maxx = $projection->{extent}{minx} + ($self->{parameters}{tilecol}+1) * $width;
        my $miny = $projection->{extent}{maxy} - ($self->{parameters}{tilerow}+1) * $height;

        {
            use bytes;
            $file = "/vsistdout";
            my $pixel_width = $width / $tile_width;
            my $pixel_height = $height / $tile_height;
            my $stdout = capture_stdout {
                if ($set->{processing}) {
                    $tile_width += 2;
                    $tile_height += 2;
                    $minx -= $pixel_width;
                    $maxy += $pixel_height;
                    $maxx += $pixel_width; 
                    $miny -= $pixel_height;
                    $file = "/vsimem/tmp.png"; # should be unique?
                }
                $ds->Translate($file, ['-of' => 'PNG', '-r' => 'bilinear' , 
                                       '-outsize' , $tile_width, $tile_height, 
                                       '-projwin', $minx, $maxy, $maxx, $miny]);
                if ($set->{processing}) {
                    $ds = Geo::GDAL::Open($file);
                    $file = "/vsimem/tmp2.png";
                    $ds->DEMProcessing($file, $set->{processing}, undef, { of => 'PNG' });
                    $ds = Geo::GDAL::Open($file);
                    $file = "/vsistdout";
                    $tile_width -= 2;
                    $tile_height -= 2;
                    $minx += $pixel_width;
                    $maxy -= $pixel_height;
                    $maxx -= $pixel_width; 
                    $miny += $pixel_height;
                    $ds->Translate($file, ['-of' => 'PNG', '-r' => 'bilinear' , 
                                           '-outsize' , $tile_width, $tile_height, 
                                           '-projwin', $minx, $maxy, $maxx, $miny]);
                }
            };
        
            return $self->{responder}->([ 200, 
                                          [
                                           'Content-Type'   => $set->{Format},
                                           'Content-Length' => length($stdout)
                                          ],
                                          [$stdout],
                                        ]);
        }

    } else {

        my $z = $self->{parameters}{tilematrix};
        my $y = $self->{parameters}{tilecol};
        my $x = 2**$z-$self->{parameters}{tilerow}-1;

        $file = "$set->{path}/$z/$y/$x.$set->{ext}";
        $file = $self->{config}{blank} unless -r $file;

    }   

    open my $fh, "<:raw", $file or return $self->return_403;

    my @stat = stat $file;

    Plack::Util::set_io_path($fh, Cwd::realpath($file));
    
    return $self->{responder}->([ 200, 
                           [
                            'Content-Type'   => $set->{Format},
                            'Content-Length' => $stat[7],
                            'Last-Modified'  => HTTP::Date::time2str( $stat[9] )
                           ],
                           $fh,
                         ]);
}

=pod

=head3 TMS

Sends TileMapService (layer not in the URL) or TileMap (layer in the
URL but no zoom, row, col) document, or the requested tile based on
layer, zoom, row, and column in the URL.

=cut

sub TMS {
    my ($self) = @_;
    my ($layer) = $self->{env}{PATH_INFO} =~ /^\/(\w+)/; # /ilmakuvat/10/577/735.png
    return $self->tilemaps unless defined $layer;
    my (undef, $zxy, $ext) = $self->{env}{PATH_INFO} =~ /^\/(\w+)\/(.*?)\.(\w+)$/; # /ilmakuvat/10/577/735.png
    my $set;
    for my $s (@{$self->{config}{TileSets}}) {
        $set = $s, last if $s->{Layers} eq $layer;
    }
    return $self->return_403 unless defined $set;
    $ext = $set->{ext} unless $ext;
    return $self->tilemapresource($set) unless defined $zxy;
    
    my $file = "$set->{path}/$zxy.$ext";
    $file = $self->{config}{blank} unless -r $file;

    open my $fh, "<:raw", $file or return $self->return_403;

    my @stat = stat $file;
    
    Plack::Util::set_io_path($fh, Cwd::realpath($file));
    
    $self->{responder}->([ 200, 
                           [
                            'Content-Type'   => 'image/'.$ext,
                            'Content-Length' => $stat[7],
                            'Last-Modified'  => HTTP::Date::time2str( $stat[9] )
                           ],
                           $fh,
                         ]);
}

sub tile_matrix_set {
    my ($writer, $projection, $tile_matrix_set) = @_;
    $writer->element('ows:Identifier' => $projection->{identifier});
    $writer->element(BoundingBox => { crs =>  $projection->{crs} },
                     [ ['ows:LowerCorner' => $projection->{extent}{minx}.' '.$projection->{extent}{miny} ],
                       ['ows:UpperCorner' => $projection->{extent}{maxx}.' '.$projection->{extent}{maxy} ] ]);
    $writer->element('ows:SupportedCRS' => { crs =>  $projection->{crs} });
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
}

sub tilemaps {
    my ($self) = @_;
    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();
    $writer->open_element(TileMapService => { version => "1.0.0", tilemapservice => "http://tms.osgeo.org/1.0.0" });
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
    $writer->open_element(TileMap => { version => "1.0.0", tilemapservice => "http://tms.osgeo.org/1.0.0" });
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

sub return_403 {
    my $self = shift;
    $self->{responder}->([403, ['Content-Type' => 'text/plain', 'Content-Length' => 9], ['forbidden']]);
}

sub FeatureInfo {
    my ($self) = @_;
    $self->return_403;
}

sub error {
    my ($self, $msg) = @_;
    if (!$msg->{debug}) {
        Geo::OGC::Service::error($self->{responder}, $msg);
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
