module Tracks.Encoder exposing (trackListValue)

import Array exposing (Array)
import Json.Encode exposing (..)
import Tracks.Track exposing (..)


pointValue : Int -> Point -> Value
pointValue lvl p =
    object
        [ ( "x", float p.x )
        , ( "y", float p.y )
        , ( "active", bool p.active )
        ]


inscribedCircleValue : Int -> InscribedCircle -> Value
inscribedCircleValue lvl c =
    object
        [ ( "cx", float c.cx )
        , ( "cy", float c.cy )
        , ( "width", float c.width )
        , ( "height", float c.height )
        , ( "angle", float c.angle )
        , ( "active", bool c.active )
        ]


rectangleRegionValue : Int -> RectangleRegion -> Value
rectangleRegionValue lvl r =
    object
        [ ( "x", float r.x )
        , ( "y", float r.y )
        , ( "width", float r.width )
        , ( "height", float r.height )
        , ( "active", bool r.active )
        ]


trackDatavalue : Int -> TrackData -> Value
trackDatavalue lvl data =
    (case data of
        PointTrack points ->
            Array.map (pointValue (lvl + 4)) points

        InscribedCircleTrack circles ->
            Array.map (inscribedCircleValue (lvl + 4)) circles

        RectangleRegionTrack rectangles ->
            Array.map (rectangleRegionValue (lvl + 4)) rectangles

        Empty ->
            Array.empty
    )
        |> array


trackValue : Int -> Track -> Value
trackValue lvl track =
    let
        trackType =
            case track.data of
                PointTrack _ ->
                    "point"

                InscribedCircleTrack _ ->
                    "inscribed_circle"

                RectangleRegionTrack _ ->
                    "rectangle_region"

                Empty ->
                    "empty"
    in
    object
        [ ( "name", string track.name )
        , ( "type", string trackType )
        , ( "data", trackDatavalue (lvl + 4) track.data )
        ]


trackListValue : TrackFile -> Value
trackListValue trackFile =
    object
        [ ( "length", int trackFile.length )
        , ( "video_resolution"
          , object
                [ ( "width", int trackFile.width )
                , ( "height", int trackFile.height )
                ]
          )
        , ( "tracks", List.map (trackValue 0) trackFile.tracks |> list )
        ]
