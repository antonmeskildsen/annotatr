module Tracks.Track exposing (InscribedCircle, Point, RectangleRegion, Track, TrackData(..), TrackFile, TrackStyle, cross, defaultInscribedCircle, defaultPoint, defaultRectangleRegion, drawTrack, drawTrackList, inscribedCircle, point, printTrackDataType, rectangleRegion)

import Array exposing (Array)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAt


type alias Point =
    { x : Float
    , y : Float
    , active : Bool
    }


type alias InscribedCircle =
    { cx : Float
    , cy : Float
    , width : Float
    , height : Float
    , angle : Float
    , active : Bool
    }


type alias RectangleRegion =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , active : Bool
    }


type alias TrackStyle =
    { primaryColor : String
    , secondaryColor : String
    }



--type TrackFile = --List (Track, (a -> TrackStyle -> Svg.Svg msg))
--    { tracks: List Track
--    , drawFuncs: List (a -> TrackStyle -> Svg.Svg msg)
--    }


type TrackData
    = PointTrack (Array Point)
    | InscribedCircleTrack (Array InscribedCircle)
    | RectangleRegionTrack (Array RectangleRegion)
    | Empty


type alias Track =
    { data : TrackData
    , style : TrackStyle
    , name : String
    }


type alias TrackFile =
    { tracks : List Track
    , length : Int
    , width : Int
    , height : Int
    }


defaultPoint : Point
defaultPoint =
    { x = 300
    , y = 300
    , active = True
    }


defaultInscribedCircle : InscribedCircle
defaultInscribedCircle =
    { cx = 300
    , cy = 300
    , width = 50
    , height = 50
    , angle = 0
    , active = True
    }


defaultRectangleRegion : RectangleRegion
defaultRectangleRegion =
    { x = 100
    , y = 100
    , width = 100
    , height = 50
    , active = True
    }


printTrackDataType : TrackData -> String
printTrackDataType trackdata =
    case trackdata of
        PointTrack _ ->
            "PointTrack"

        InscribedCircleTrack _ ->
            "InscribedCircleTrack"

        RectangleRegionTrack _ ->
            "RectangleRegionTrack"

        Empty ->
            "Empty"


drawTrackList : Int -> List Track -> List (Svg.Svg msg)
drawTrackList idx tracks =
    List.map (drawTrack idx) tracks


drawTrack : Int -> Track -> Svg.Svg msg
drawTrack idx track =
    let
        createElement func array =
            case Array.get idx array of
                Just a ->
                    case a.active of
                        True ->
                            func track.style a

                        False ->
                            Svg.svg [] []

                Nothing ->
                    Svg.svg [] []
    in
    case track.data of
        PointTrack points ->
            createElement point points

        InscribedCircleTrack circles ->
            createElement inscribedCircle circles

        RectangleRegionTrack rectangles ->
            createElement rectangleRegion rectangles

        Empty ->
            Svg.svg [] []


cross : Int -> Int -> Int -> Int -> String -> Svg.Svg msg
cross cx cy w h color =
    let
        x =
            cx - w // 2

        y =
            cy - w // 2

        l1_x =
            toString (w // 2)

        l1_y2 =
            toString h

        l2_y =
            toString (h // 2)

        l2_x2 =
            toString w
    in
    Svg.svg
        [ SvgAt.x (toString x), SvgAt.y (toString y), SvgAt.width l2_x2, SvgAt.height l1_y2 ]
        [ Svg.line
            [ SvgAt.x1 l1_x
            , SvgAt.x2 l1_x
            , SvgAt.y1 "0"
            , SvgAt.y2 l1_y2
            , SvgAt.stroke color
            , SvgAt.strokeWidth "2"
            ]
            []
        , Svg.line
            [ SvgAt.x1 "0"
            , SvgAt.x2 l2_x2
            , SvgAt.y1 l2_y
            , SvgAt.y2 l2_y
            , SvgAt.stroke color
            , SvgAt.strokeWidth "2"
            ]
            []

        --, Svg.circle [ SvgAt.r "30", SvgAt.cx "15", SvgAt.cy "15"] []
        ]


point : TrackStyle -> Point -> Svg.Svg msg
point info point =
    let
        color =
            info.primaryColor
    in
    Svg.svg [] [ cross (round point.x) (round point.y) 30 30 color ]


inscribedCircle : TrackStyle -> InscribedCircle -> Svg.Svg msg
inscribedCircle info c =
    let
        rx =
            c.width / 2

        ry =
            c.height / 2

        strAngle =
            toString c.angle

        strCx =
            toString c.cx

        strCy =
            toString c.cy

        strWidth =
            toString c.width

        strHeight =
            toString c.height

        size =
            2 + max c.width c.height

        center =
            size / 2

        strSize =
            size |> toString

        strCenter =
            center |> toString

        trans =
            "rotate (" ++ strAngle ++ " " ++ strCenter ++ " " ++ strCenter ++ ")"
    in
    Svg.svg
        [ SvgAt.x (c.cx - center |> toString)
        , SvgAt.y (c.cy - center |> toString)
        , SvgAt.width strSize
        , SvgAt.height strSize
        ]
        [ Svg.g
            [ SvgAt.transform trans ]
            [ Svg.ellipse
                [ SvgAt.cx strCenter
                , SvgAt.cy strCenter
                , SvgAt.rx (rx |> toString)
                , SvgAt.ry (ry |> toString)
                , SvgAt.stroke info.secondaryColor
                , SvgAt.strokeWidth "2"
                , SvgAt.fillOpacity "0"
                ]
                []
            , cross (round center) (round center) 10 10 info.primaryColor
            ]
        ]


rectangleRegion : TrackStyle -> RectangleRegion -> Svg.Svg msg
rectangleRegion info rectangle =
    let
        strX =
            toString rectangle.x

        strY =
            toString rectangle.y

        strWidth =
            toString rectangle.width

        strHeight =
            toString rectangle.height
    in
    Svg.svg
        [ SvgAt.x strX
        , SvgAt.y strY
        , SvgAt.width strWidth
        , SvgAt.height strHeight
        ]
        [ Svg.rect
            [ SvgAt.x "0"
            , SvgAt.y "0"
            , SvgAt.width strWidth
            , SvgAt.height strHeight
            , SvgAt.stroke info.secondaryColor
            , SvgAt.strokeWidth "2"
            , SvgAt.fillOpacity "0"
            ]
            []
        ]
