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

use 5.010000; # // and //=
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
        $self->log({ request => $self->{request}, parameters => $self->{parameters} });
        my $parser = XML::LibXML->new(no_blanks => 1);
        my $pp = XML::LibXML::PrettyPrint->new(indent_string => "  ");
        if ($self->{posted}) {
            my $dom = $parser->load_xml(string => $self->{posted});
            $pp->pretty_print($dom); # modified in-place
            say STDERR "posted:\n",$dom->toString;
        }
        if ($self->{filter}) {
            my $dom = $parser->load_xml(string => $self->{filter});
            $pp->pretty_print($dom); # modified in-place
            say STDERR "filter:\n",$dom->toString;
        }
    }
    for ($self->{request}{request} // '') {
        if (/^GetCapabilities/ or /^capabilities/) { $self->GetCapabilities() }
        elsif (/^GetTile/)                         { $self->GetTile() }
        elsif (/^FeatureInfo/)                     { $self->FeatureInfo() }
        elsif (/^$/)                               { 
            $self->error({ exceptionCode => 'MissingParameterValue',
                           locator => 'request' }) }
        else                                       { 
            $self->error({ exceptionCode => 'InvalidParameterValue',
                           locator => 'request',
                           ExceptionText => "$self->{parameters}{request} is not a known request" }) }
    }
}

sub GetCapabilities {
    my ($self) = @_;

    my $writer = Geo::OGC::Service::XMLWriter::Caching->new();

    $writer->open_element(WMT_MS_Capabilities => { version => '1.1.1' });
    $writer->element(Service => [[Name => 'OGC:WMS'],
                                 ['Title'],
                            [OnlineResource => {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                                                'xlink:href' => $my_url}]]);
    $writer->open_element('Capability');
    $writer->element(Request => 
                     [[GetCapabilities => 
                       [[Format => 'application/vnd.ogc.wms_xml'],
                        [DCPType => 
                         [HTTP => 
                          [Get => 
                           [OnlineResource => 
                            {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                             'xlink:href' => $my_url}]]]]]],
                      [GetMap => 
                       [[Format => 'image/png'],
                        [DCPType => 
                         [HTTP => 
                          [Get => 
                           [OnlineResource => 
                            {'xmlns:xlink' => "http://www.w3.org/1999/xlink",
                             'xlink:href' => $my_url}]]]]]]
                     ]);
    $writer->element(Exception => [Format => 'text/plain']);
    
    for my $set (@{$config->{TileSets}}) {
        my($i0,$i1) = split /\.\./, $set->{Resolutions};
        my @resolutions = @resolutions_3857[$i0..$i1];
        $writer->element(VendorSpecificCapabilities => 
                         [TileSet => [[SRS => $set->{SRS}],
                                      [BoundingBox => $set->{BoundingBox}],
                                      [Resolutions => "@resolutions"],
                                      [Width => $set->{Width} || 256],
                                      [Height => $set->{Height} || 256],
                                      [Format => $set->{Format}],
                                      [Layers => $set->{Layers}],
                                      [Styles => undef]]]);
    }

    $writer->element(UserDefinedSymbolization => 
                     {SupportSLD => 0, UserLayer => 0, UserStyle => 0, RemoteWFS => 0});

    for my $set (@{$config->{TileSets}}) {
        $writer->element(Layer => [[Title => 'TileCache Layers'],
                                   [Layer => {queryable => 0, opaque => 0, cascaded => 1}, 
                                    [[Name => $set->{Layers}],
                                     [Title => $set->{Layers}],
                                     [SRS => $set->{SRS}],
                                     [BoundingBox => $set->{BoundingBox}]]]
                         ]);
    }

    $writer->close_element;
    $writer->close_element;
    $writer->stream($self->{responder});
}

sub GetTile {
    my ($self) = @_;
    my ($set, $zxy, $ext);
    my $layers = $self->{parameters}{layers};
    ($layers, $zxy, $ext) = $self->{env}{PATH_INFO} =~ /(\w+)\/(.*?)\.(\w+)$/ unless $layers;
    for my $s (@{$self->{config}{TileSets}}) {
        $set = $s, last if $s->{Layers} eq $layers;
    }
    $ext = $set->{ext} unless $ext;

#    if ($request eq 'GetMap') {
    {
        my $bbox = $self->{parameters}{bbox};
        my @bbox = split /,/, $bbox; # minx, miny, maxx, maxy
        my $units_per_pixel = ($bbox[2]-$bbox[0])/256;
        my $z;
        my $res;
        for my $r (@resolutions_3857) {
            if (abs($r - $units_per_pixel) < 0.1) {
                $res = $r;
                $z = $i;
                last;
            }
        }
        my $rows = 2**$z;

        #my $wh = ($bounding_box_3857->{maxx} - $bounding_box_3857->{minx})/$rows;
        #my $x = ($bbox[2]+$bbox[0])/2 - $bounding_box_3857->{minx};
        #my $y = ($bbox[3]+$bbox[1])/2 - $bounding_box_3857->{miny};
        #$x = int($x / $wh);
        #$y = int($y / $wh);

        my $x = int(($bbox[0] - $bounding_box_3857->{minx}) / ($res * 256) + 0.5);
        my $y = int(($bbox[1] - $bounding_box_3857->{miny}) / ($res * 256) + 0.5);
        $zxy = "$z/$x/$y";
    }
#    }

    my $file = "$set->{path}/$zxy.$ext";

    open my $fh, "<:raw", $file or $self->return_403;

    my @stat = stat $file;

    Plack::Util::set_io_path($fh, Cwd::realpath($file));
    
    $self->{responder}->([ 200, 
                           [
                            'Content-Type'   => "image/$ext",
                            'Content-Length' => $stat[7],
                            'Last-Modified'  => HTTP::Date::time2str( $stat[9] )
                           ],
                           $fh,
                         ]);
}

sub return_403 {
    my $self = shift;
    $self->{responder}->([403, ['Content-Type' => 'text/plain', 'Content-Length' => 9], ['forbidden']]);
}

sub FeatureInfo {
    my ($self) = @_;
    $self->return_403;
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
