module Tracks.Decoder exposing (decodeInscribedCircle, decodePoint, decodeRectangleRegion, decodeTrackData, decodeTrackEx, decodeTrackFile, testDecoder)

--import Json.Decode.Pipeline exposing (..)

import Array exposing (Array)
import Debug
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Tracks.Track exposing (..)


decodePoint : Decoder Point
decodePoint =
    decode Point
        |> required "x" float
        |> required "y" float
        |> required "active" bool


decodeInscribedCircle : Decoder InscribedCircle
decodeInscribedCircle =
    decode InscribedCircle
        |> required "cx" float
        |> required "cy" float
        |> required "width" float
        |> required "height" float
        |> required "angle" float
        |> required "active" bool


decodeRectangleRegion : Decoder RectangleRegion
decodeRectangleRegion =
    decode RectangleRegion
        |> required "x" float
        |> required "y" float
        |> required "width" float
        |> required "height" float
        |> required "active" bool


testDecoder : Decoder (Array Point)
testDecoder =
    at [ "data", "pupil_center" ] (array decodePoint)


decodeTrackFile : Decoder TrackFile
decodeTrackFile =
    decode TrackFile
        |> required "tracks" (list decodeTrackEx)
        |> required "length" int
        |> requiredAt [ "video_resolution", "width" ] int
        |> requiredAt [ "video_resolution", "height" ] int



--type TrackType
--    = Point
--    | InscribedCircle
--    | None
-- trackTypeFromString : String -> TrackType
-- trackTypeFromString typeString =
--     case typeString of
--         "point" ->
--             Point
--         "inscribed_circle" ->
--             InscribedCircle
--         _ ->
--             None


decodeTrackEx : Decoder Track
decodeTrackEx =
    decode Track
        |> custom (field "type" string |> andThen decodeTrackData)
        |> hardcoded { primaryColor = "red", secondaryColor = "blue" }
        |> required "name" string


decodeTrackData : String -> Decoder TrackData
decodeTrackData t =
    case t of
        "point" ->
            map PointTrack (field "data" (array decodePoint))

        "inscribed_circle" ->
            map InscribedCircleTrack (field "data" (array decodeInscribedCircle))

        "rectangle_region" ->
            map RectangleRegionTrack (field "data" (array decodeRectangleRegion))

        "empty" ->
            succeed Empty

        _ ->
            fail "hejsa"
