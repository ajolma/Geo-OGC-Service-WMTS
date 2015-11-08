=pod

=head1 NAME

Geo::OGC::Service::WMTS - Perl extension to create geospatial web map tile services

=head1 SYNOPSIS

The process_request method of this module is called by the
Geo::OGC::Service framework.

=head1 DESCRIPTION

This module aims to provide the operations defined by the Open
Geospatial Consortium's Web Map Tile Service standard.

This module is a plugin for the Geo::OGC::Service framework.

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

our $VERSION = '0.01';

# http://www.microimages.com/documentation/TechGuides/78googleMapsStruc.pdf
# pixel size at equator in meters
our @resolutions_3857 = (156543.03390000000945292413,
                        78271.51695000000472646207,
                        39135.75847500000236323103,
                        19567.87923750000118161552,
                        9783.93961875000059080776,
                        4891.96980937500029540388,
                        2445.98490468750014770194,
                        1222.99245234375007385097,
                        611.49622617187503692548,
                        305.74811308593751846274,
                        152.87405654296875923137,
                        76.43702827148437961569,
                        38.21851413574218980784,
                        19.10925706787109490392,
                        9.55462853393554745196,
                        4.77731426696777372598,
                        2.38865713348388686299,
                        1.19432856674194343150,
                        0.59716428337097171575,
                        0.29858214168548585787);

our $bounding_box_3857 = {SRS => 'EPSG:3857', 
                         minx => -20037508.34,
                         miny => -20037508.34,
                         maxx => 20037508.34,
                         maxy => 20037508.34};

=pod

=head3 process_request

The entry method into this service. Fails unless the request is well known.

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
    if ($self->{debug} > 2) {
        $self->log($self);
    }
    my $response;
    for ($self->{request} // '') {
        if (/^GetCapabilities/ or /^capabilities/) { $self->GetCapabilities() }
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

sub GetCapabilities {
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
        my @resolutions = @resolutions_3857;
        $writer->element(VendorSpecificCapabilities => 
                         [TileSet => [[SRS => $set->{SRS}],
                                      #[BoundingBox => $set->{BoundingBox}], # with this QGIS does not show tiles at correct locations
                                      [BoundingBox => $bounding_box_3857],
                                      [Resolutions => "@resolutions"],
                                      [Width => $set->{Width} || 256],
                                      [Height => $set->{Height} || 256],
                                      [Format => $set->{Format}],
                                      [Layers => $set->{Layers}],
                                      [Styles => undef]]]);
    }

    $writer->element(UserDefinedSymbolization => 
                     {SupportSLD => 0, UserLayer => 0, UserStyle => 0, RemoteWFS => 0});

    for my $set (@{$self->{config}{TileSets}}) {
        $writer->element(Layer => [[Title => 'TileCache Layers'],
                                   [Layer => {queryable => 0, opaque => 0, cascaded => 1}, 
                                    [[Name => $set->{Layers}],
                                     [Title => $set->{Layers}],
                                     [SRS => $set->{SRS}],
                                     [Format => $set->{Format}],
                                     [BoundingBox => $bounding_box_3857],
                                     #[BoundingBox => $set->{BoundingBox}] # with this QGIS does not show tiles at correct locations
                                    ]]
                         ]);
    }

    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

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
    
    my @bbox = split /,/, $self->{parameters}{bbox}; # minx, miny, maxx, maxy
    my $units_per_pixel = ($bbox[2]-$bbox[0])/256;
    my $z;
    my $res;
    my $i = 0;
    for my $r (@resolutions_3857) {
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

    my $tx = int( POSIX::ceil( $px / 256.0 ) - 1 );
    my $ty = int( POSIX::ceil( $py / 256.0 ) - 1 );

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


    my $layer = $self->{parameters}{layers};

    # get x, y, z from tilerow tilecol tilematrix tilematrixset
    my $x;
    my $y;
    my $z;

    if ($self->{parameters}{tilematrixset} eq 'EPSG:3857') {
        $z = $self->{parameters}{tilematrix};
        $y = $self->{parameters}{tilecol};
        $x = 2**$z-$self->{parameters}{tilerow}-1;
    }
    

    my $file = "$set->{path}/$z/$y/$x.$set->{ext}";
    $file = $self->{config}{blank} unless -r $file;

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
    $writer->element(TileFormat => {width => 256, height => 256, 'mime-type' => $set->{Format}, extension => $ext });
    my @sets;
    my ($n, $m) = $set->{Resolutions} =~ /(\d+)\.\.(\d+)$/;
    for my $i ($n..$m) {
        push @sets, [TileSet => {href=>$i, order=>$i, 'units-per-pixel'=>$resolutions_3857[$i]}];
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
