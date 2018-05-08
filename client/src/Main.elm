port module Main exposing (..)

import Maybe exposing (..)
import Array
import Css exposing (position, absolute, width, height, top, left, px, pct)
import Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as Json exposing (Decoder)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAt

import Tracks.Track exposing (..)
import Tracks.Decoder exposing (..)
import Tracks.Encoder exposing (..)
import Window.Events exposing (onWindow)

import String.Format exposing (..)
import Dom
import Task

import Select


getOrDefault : Int -> a -> Array.Array a -> a
getOrDefault i default array =
    case Array.get i array of
        Just a ->
            a

        Nothing ->
            default


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mediaUrl : String
    , videoFile : String
    , mediaType : String
    , framesPerSecond : Int
    , newFps : String
    , currentFrame : Int
    , frameToSet : Int
    , tracks : TrackFile
    , trackLength : Int
    , videoWidth : Int
    , videoHeight : Int
    , trackFile : String
    , newTrackName : String
    , newTrackType : String
    , currentPos : Point
    , trackFocus : Int
    , infoMsg : String
    }



-- UPDATE


init : ( Model, Cmd Msg )
init =
    { mediaUrl = "https://pupil-data.ams3.digitaloceanspaces.com/videos/anton/1/eyes.webm"
    , videoFile = ""
    , mediaType = "video/webm"
    , newFps = "60"
    , framesPerSecond = 60
    , currentFrame = 0
    , frameToSet = 0
    , tracks = { tracks=[], length=0, width=0, height=0 }
    , trackLength = 0
    , videoWidth = 0
    , videoHeight = 0
    , trackFile = "f3.json"
    , newTrackName = ""
    , newTrackType = "Point"
    , currentPos = ( 0, 0 )
    , trackFocus = 0
    , infoMsg = "Ok"
    }
        ! [ startFrameUpdater (), getVideoMeta () ]

type UpdateElement
    = PointElement Point
    | InscribedCircleElement InscribedCircle
    | RectangleRegionElement RectangleRegion

type Msg
    = NoOp
    | TimeUpdate Float
    | SetPlayerTime Float
    | HandleKeyboardEvent KeyboardEvent
    | PrevFrame
    | NextFrame
    | UpdateFrame
    | SetNextFrame String
    | SetTrackFileName String
    | LoadTrackFile
    | SaveTrackFile
    | NewTrackFile
    | TrackSavedSuccess (Result Http.Error Bool)
    | SetNewTrackName String
    | SetNewTrackType String
    | SetTrackFocus Int
    | AddTrack
    | CopyTrack Int
    | RemoveTrack Int
    | NewTrackFromFile (Result Http.Error TrackFile)
    | SetVideoFile String
    | LoadVideo
    | SetVideoMeta (Float, Int, Int)
    | UpdateTrackElement Int Int UpdateElement
    | CopyRest Int Int UpdateElement
    | HandleEditKeyboardEvent Int KeyboardEvent
    | SetNewFps String
    | SetActualFps
    | FocusOn String
    | FocusResult (Result Dom.Error ())


removeTrackIdx : List Track -> Int -> List Track
removeTrackIdx before idx =
    (List.take idx before) ++ (List.drop (idx+1) before)

replaceListIdx : List a -> Int -> a -> List a
replaceListIdx before idx replacement  =
    (List.take idx before) ++ (replacement :: (List.drop (idx+1) before))

getListIdx : Int -> List a -> Maybe a
getListIdx idx l =
    List.drop idx l |> List.head

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            let
                mult =
                    if event.shiftKey then
                        10
                    else
                        1
            in
            case event.keyCode of
                Keyboard.Key.P ->
                    ( model, togglePlaying () )

                Keyboard.Key.U ->
                    deltaFrame model -mult

                Keyboard.Key.O ->
                    deltaFrame model mult

                _ ->
                    ( model, Cmd.none )

        SetPlayerTime newTime ->
            ( model, setCurrentTime newTime )

        PrevFrame ->
            deltaFrame model -1

        NextFrame ->
            deltaFrame model 1
        
        SetNextFrame num ->
            case String.toInt num of
                Ok i ->
                    ( { model | frameToSet = i }, Cmd.none )
                Err _ ->
                    ( { model | infoMsg = "invalid frame format" }, Cmd.none)
        
        UpdateFrame ->
            ( { model | currentFrame = model.frameToSet, infoMsg = "Ok" }, setCurrentTime (frameToTime model.frameToSet model.framesPerSecond) )

        TimeUpdate time ->
            ( { model | currentFrame = timeToFrames time model.framesPerSecond }, Cmd.none )

        SetTrackFileName name ->
            ( { model | trackFile = name }, Cmd.none)

        LoadTrackFile ->
            ( model, loadTrack model.trackFile )

        SaveTrackFile ->
            ( model, saveTrack model.trackFile model.tracks )
        
        NewTrackFile ->
            let
                _ = Debug.log "hej" model.trackLength 
            in 
            ( { model | tracks = { tracks = [], length = model.trackLength, width = model.videoWidth, height = model.videoHeight }}, Cmd.none)
        
        TrackSavedSuccess v ->
            ( { model | infoMsg = "save successful" }, Cmd.none)

        SetNewTrackName name ->
            ( { model | newTrackName = name }, Cmd.none )

        SetNewTrackType trackType ->
            ( { model | newTrackType = trackType }, Cmd.none )
        
        SetTrackFocus idx ->
            let
                id_ = "task_" ++ toString idx
            in
                
            ( { model | trackFocus = idx }, Dom.focus id_ |> Task.attempt FocusResult )
        
        SetVideoMeta (duration, width, height) ->
            let
                _ = Debug.log "frames:" (timeToFrames duration model.framesPerSecond)
                mtrack = model.tracks
                len = timeToFrames duration model.framesPerSecond
                new = { mtrack | length=len }
            in
                
                ( { model | tracks=new, trackLength=len, videoWidth=width, videoHeight=height }, Cmd.none)

        AddTrack ->
            let
                _ = Debug.log "type" model.newTrackType
                data =
                    case model.newTrackType of
                        "Point" ->
                            Array.repeat model.tracks.length defaultPoint |> PointTrack
                        "Inscribed Circle" ->
                            Array.repeat model.tracks.length defaultInscribedCircle |> InscribedCircleTrack
                        "Rectangle Region" ->
                            Array.repeat model.tracks.length defaultRectangleRegion |> RectangleRegionTrack
                        _ ->
                            Empty

                new = { data=data, style={ primaryColor="red", secondaryColor="blue"}, name=model.newTrackName}
            in
                ( { model | tracks = { length=model.tracks.length
                                     , width = model.tracks.width
                                     , height = model.tracks.height
                                     , tracks = new :: model.tracks.tracks}  }, Cmd.none )
        
        CopyTrack idx ->
            let
                new = List.drop idx model.tracks.tracks |> List.head 
            in 
                case new of
                    Just a ->
                        ( { model | tracks = { length=model.tracks.length
                                             , width = model.tracks.width
                                             , height = model.tracks.height
                                             , tracks = a :: model.tracks.tracks} }, Cmd.none )
                    Nothing ->
                        ( model, Debug.log "Couldn't copy track, invalid index" Cmd.none)
        RemoveTrack idx -> 
            ( { model | tracks = { length=model.tracks.length
                                 , tracks = removeTrackIdx model.tracks.tracks idx 
                                 , width = model.tracks.width
                                 , height = model.tracks.height }}, Cmd.none )

        NewTrackFromFile res ->
            case res of
                Ok parse_res ->     
                    ( {model | tracks = parse_res}, Cmd.none )

                Err err ->
                    let 
                        _ = Debug.log "parse err" err
                    in
                        ( { model | infoMsg="Couldn't load track"}, Cmd.none )

        SetVideoFile name ->
            ( { model | videoFile=name }, Cmd.none )
        
        LoadVideo ->
            ( { model | mediaUrl=model.videoFile }, getVideoMeta () )

        UpdateTrackElement frame trackIdx element ->
            case (getListIdx trackIdx model.tracks.tracks) of
                Just track ->
                    updateTrackElement model track frame trackIdx element
                Nothing ->
                    (model, Debug.log "Couldn't update track" Cmd.none)
        
        CopyRest frame trackIdx element ->
            case (getListIdx trackIdx model.tracks.tracks) of
                Just track ->
                    updateTrackElementRange model track frame trackIdx element
                Nothing ->
                    (model, Debug.log "Couldn't update track" Cmd.none)

        HandleEditKeyboardEvent idx event ->
            let
                _ = Debug.log "ahhhh"  
            in 
            case (getListIdx idx model.tracks.tracks) of 
                Just track ->
                    case event.keyCode of
                        Keyboard.Key.C ->
                            case track.data of
                                PointTrack points ->
                                    case Array.get model.currentFrame points of
                                        Just point ->
                                            updateTrackElement model track (model.currentFrame+1) idx (PointElement point)
                                        Nothing ->
                                            ( model, Debug.log "hallo" Cmd.none )
                                InscribedCircleTrack circles ->
                                    case Array.get model.currentFrame circles of
                                        Just circ ->
                                            updateTrackElement model track (model.currentFrame+1) idx (InscribedCircleElement circ)
                                        Nothing ->
                                            ( model, Debug.log "hejsa" Cmd.none )
                                RectangleRegionTrack rectangles ->
                                    case Array.get model.currentFrame rectangles of
                                        Just rectangle ->
                                            updateTrackElement model track (model.currentFrame+1) idx (RectangleRegionElement rectangle)
                                        Nothing ->
                                            ( model, Debug.log "wee" Cmd.none)
                                _ -> 
                                    ( model, Debug.log "hej" Cmd.none )
                        _ ->
                            handleFeatureMove model track idx event

                Nothing ->
                    ( model, Debug.log "Invalid track index" Cmd.none )

        SetNewFps fps ->
            ( { model | newFps=fps }, Cmd.none )

        SetActualFps ->
            case String.toInt model.newFps of
                Ok fpsStr ->
                    ( { model | framesPerSecond=fpsStr }, Cmd.none)
                
                Err err ->
                    ( model, Debug.log "could not convert string to int (fps)" Cmd.none)
        
        FocusOn id ->
            ( model, Dom.focus id |> Task.attempt FocusResult )
        
        FocusResult res ->
            case res of
                Ok () ->
                    (model, Debug.log "focus success" Cmd.none )
                Err err ->
                    let
                        _ = Debug.log "focus error " err
                    in
                        (model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

updateTrackElement : Model -> Track -> Int -> Int -> UpdateElement -> (Model, Cmd Msg)
updateTrackElement model track frame trackIdx element =
    let
        newTrackData = setElement frame element track.data
        newTrack = { track | data = newTrackData }
        newTrackList = replaceListIdx model.tracks.tracks trackIdx newTrack
        --newTrackArray = Array.set idx newTrackData model.tracks.tracks
        mt = model.tracks
        newTrackFile = { mt | tracks = newTrackList }
    in 
        ( {model | tracks = newTrackFile }, Cmd.none )

updateTrackElementRange : Model -> Track -> Int -> Int -> UpdateElement -> (Model, Cmd Msg)
updateTrackElementRange model track frame trackIdx element =
    let
        newTrackData = List.foldl (\i acc -> setElement i element acc) track.data (List.range frame model.trackLength)
        newTrack = { track | data = newTrackData }
        newTrackList = replaceListIdx model.tracks.tracks trackIdx newTrack
        
        --newTrackArray = Array.set idx newTrackData model.tracks.tracks
        mt = model.tracks
        newTrackFile = { mt | tracks = newTrackList }
    in 
        ( {model | tracks = newTrackFile }, Cmd.none )

handleFeatureMove : Model -> Track -> Int -> KeyboardEvent -> (Model, Cmd Msg)
handleFeatureMove model track idx event =
    let 
        mult =
            if event.shiftKey then
                10
            else
                1
        newTrackData = 
            case track.data of
                PointTrack points ->
                    case Array.get model.currentFrame points of
                        Just (x, y) ->
                            case event.keyCode of
                                Keyboard.Key.W ->
                                    setElement model.currentFrame ((x, y-mult) |> PointElement) track.data
                                Keyboard.Key.A ->
                                    setElement model.currentFrame ((x-mult, y) |> PointElement) track.data
                                Keyboard.Key.S ->
                                    setElement model.currentFrame ((x, y+mult) |> PointElement) track.data
                                Keyboard.Key.D ->
                                    setElement model.currentFrame ((x+mult, y) |> PointElement) track.data
                                _ ->
                                    track.data
                        Nothing ->
                            track.data
                InscribedCircleTrack circles ->
                    case Array.get model.currentFrame circles of
                        Just circ ->
                            case event.keyCode of
                                Keyboard.Key.W ->
                                    setElement model.currentFrame ({circ | cy = circ.cy-mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.A ->
                                    setElement model.currentFrame ({circ | cx = circ.cx-mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.S ->
                                    setElement model.currentFrame ({circ | cy = circ.cy+mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.D ->
                                    setElement model.currentFrame ({circ | cx = circ.cx+mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.H ->
                                    setElement model.currentFrame ({circ | width = circ.width-mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.J ->
                                    setElement model.currentFrame ({circ | height = circ.height-mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.K ->
                                    setElement model.currentFrame ({circ | height = circ.height+mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.L ->
                                    setElement model.currentFrame ({circ | width = circ.width+mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.Q ->
                                    setElement model.currentFrame ({circ | angle = circ.angle-mult } |> InscribedCircleElement) track.data
                                Keyboard.Key.E ->
                                    setElement model.currentFrame ({circ | angle = circ.angle+mult } |> InscribedCircleElement) track.data
                                _ ->
                                    track.data
                        Nothing ->
                            track.data
                RectangleRegionTrack rectangles ->
                    case Array.get model.currentFrame rectangles of
                        Just rectangle ->
                            case event.keyCode of
                                Keyboard.Key.W ->
                                    setElement model.currentFrame ({rectangle | y = rectangle.y-mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.A ->
                                    setElement model.currentFrame ({rectangle | x = rectangle.x-mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.S ->
                                    setElement model.currentFrame ({rectangle | y = rectangle.y+mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.D ->
                                    setElement model.currentFrame ({rectangle | x = rectangle.x+mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.H ->
                                    setElement model.currentFrame ({rectangle | width = rectangle.width-mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.J ->
                                    setElement model.currentFrame ({rectangle | height = rectangle.height-mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.K ->
                                    setElement model.currentFrame ({rectangle | height = rectangle.height+mult } |> RectangleRegionElement) track.data
                                Keyboard.Key.L ->
                                    setElement model.currentFrame ({rectangle | width = rectangle.width+mult } |> RectangleRegionElement) track.data
                                _ ->
                                    track.data
                        Nothing ->
                            track.data
                _ -> 
                    Empty
        newTrack = { track | data = newTrackData }
        newTrackList = replaceListIdx model.tracks.tracks idx newTrack
        mt = model.tracks
        newTrackFile = { mt | tracks = newTrackList }
    in 
        ( {model | tracks = newTrackFile }, Cmd.none )

setElement : Int -> UpdateElement -> TrackData -> TrackData
setElement idx element track =
    case element of
        PointElement point ->
            case track of
                PointTrack points ->
                    Array.set idx point points |> PointTrack
                _ -> 
                    track
        InscribedCircleElement circ ->
            case track of
                InscribedCircleTrack circles ->
                    Array.set idx circ circles |> InscribedCircleTrack
                _ -> track
        RectangleRegionElement rectangle ->
            case track of
                RectangleRegionTrack rectangles ->
                    Array.set idx rectangle rectangles |> RectangleRegionTrack
                _ -> track
   

--getTrack : String -> Result Error Track
--getTrack text =
--    run (pointlist []) text

saveTrack : String -> TrackFile -> Cmd Msg
saveTrack fileName tracks =
    let
        url = 
            "http://127.0.0.1:5000/track/" ++ fileName
        request =
            Http.post url (Http.jsonBody (trackListValue tracks)) (Json.succeed True)
    in
        Http.send TrackSavedSuccess request

loadTrack : String -> Cmd Msg
loadTrack fileName =
    let
        url =
            "http://127.0.0.1:5000/track/" ++ fileName

        request =
            Http.get url decodeTrack
    in
    Http.send NewTrackFromFile request

decodeTrack : Decoder TrackFile
decodeTrack =
    decodeTrackFile



-- set time port


port setCurrentTime : Float -> Cmd msg
port getCurrentTime : (Float -> msg) -> Sub msg

port startFrameUpdater : () -> Cmd msg

port togglePlaying : () -> Cmd msg

port getVideoMeta : () -> Cmd msg
port videoMeta : ((Float, Int, Int) -> msg) -> Sub msg

deltaFrame : Model -> Int -> ( Model, Cmd Msg )
deltaFrame model delta =
    let
        nextFrame =
            model.currentFrame + delta

        newTime =
            frameToTime nextFrame model.framesPerSecond
    in
    ( { model | currentFrame = nextFrame }, setCurrentTime newTime )


frameToTime : Int -> Int -> Float
frameToTime frame fps =
    (toFloat frame + 0.1) / toFloat fps


timeToFrames : Float -> Int -> Int
timeToFrames time fps =
    Basics.round (time * toFloat fps)



-- Custom event handler


onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Json.map msg targetCurrentTime)



-- A `Json.Decoder` for grabbing `event.target.currentTime`


targetCurrentTime : Json.Decoder Float
targetCurrentTime =
    Json.at [ "target", "currentTime" ] Json.float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ onWindow "keydown" (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        , getCurrentTime TimeUpdate
        , videoMeta SetVideoMeta
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "" ]
        [ div [ class "ui top fixed menu" ]
            [ div [ class "item" ] [ h3 [] [ text "Annotatr" ]]
            , div [ class "item" ]
                [ div [ class "ui action input" ] 
                    [ input [ class "", onInput SetVideoFile, value model.videoFile ] []
                    , button [ class "ui blue button", onClick LoadVideo ] [ text "Load video" ] 
                    ]
                ]
            , div [ class "item" ]
                [ div [ class "ui action input" ]
                    [ input [ class "", onInput SetTrackFileName, value model.trackFile ] []
                    , button [ class "ui blue button", onClick LoadTrackFile ] [ text "Load track" ]
                    , button [ class "ui yellow button", onClick SaveTrackFile ] [ text "Save track" ]
                    ]
                ]
            , div [ class "item" ]
                [ button [ class "ui green button", onClick NewTrackFile ] [ text "New track" ]
                ]
            , div [ class "item" ]
                [ div [ class "ui action input" ] 
                    [ input [ style [("width", "60px")], onInput SetNewFps, value (model.newFps) ] []
                    , button [ class "ui button", onClick SetActualFps ] [ text "Set fps" ]
                    ]
                ]
            , div [ class "item" ] [ text model.infoMsg ]
            ]
        , div [ class "ui grid", style [("padding-top", "65px")]]
            [ div [ class "row" ]
                [ div [ class "five wide column" ]
                    [ h4 [ class "ui header" ] [ text "Track list" ]
                    , createTrackInfoList model]
                , div [ class "eleven wide column", style [("padding-left", "0"), ("padding-right", "0")]]
                    [ video
                        [ src model.mediaUrl
                        --, Html.Styled.Attributes.width 1280
                        --, Html.Styled.Attributes.height 720
                        , id "video-player"
                        , autoplay False
                        , type_ model.mediaType
                        , controls False
                        --, onTimeUpdate TimeUpdate
                        , css [ Css.maxHeight (px 650), Css.width (pct 100)]
                        ]
                        []
                    , drawTracks model
                    ]
                ]
            , div [ class "two column row" ] 
                [ div [ class "column" ] 
                    [ div [ class ""]
                        [ div [ class "ui label" ]
                            [ text "Frame"
                            , div [ class "detail" ] [ text (toString model.currentFrame) ]
                            ]
                        , button [ class "ui labelled icon button", onClick PrevFrame ] 
                            [ i [ class "left arrow icon" ] []
                            , text "Previous frame" ]
                        , button [ class "ui right labelled icon button", onClick NextFrame ] 
                            [ i [ class "right arrow icon" ] []
                            , text "Next frame" ]
                        , div [ class "ui action input" ] 
                            [ input [ type_ "text", value (toString model.frameToSet), onInput SetNextFrame ] [] 
                            , button [ class "ui button", onClick UpdateFrame ] [ text "Set frame" ] ]
                        ]
                    ]
                ]
            , div [ class "row" ] [ drawTrackEditingUI model ]
            ]
        
        ]
        --, div []
        --    [ input [ value ] []
        --    ]

createTrackInfoList : Model -> Html Msg
createTrackInfoList model =
    let
        infoElement idx track =
            let
                id_ = "task_" ++ toString idx
                selected =
                    if idx == model.trackFocus
                    then [("background-color", "#CFCDCD")]
                    else []
            in
                div [ id id_
                    , class "item"
                    , style selected, onClick (SetTrackFocus idx)
                    , tabindex idx
                    , Json.map (HandleEditKeyboardEvent idx) decodeKeyboardEvent |> on "keydown" 
                    ]
                    [ div [ class "right floated content" ]
                        [ div [ class "ui right floated buttons" ] 
                            [ button [ class "ui red button", onClick (RemoveTrack idx)] [ text "Remove" ]
                            , button [ class "ui green button", onClick (CopyTrack idx)] [ text "Copy" ]]
                        ]
                    , div [ class "content" ] 
                        [ div [ class "header" ] [ text track.name ]
                        , div [ class "description" ] [ text (Tracks.Track.printTrackDataType track.data)]
                        ]
                    ]
    in
        div [ class "ui middle aligned divided selection list"] 
            (List.indexedMap infoElement model.tracks.tracks
            ++
            [ div [ class "ui form" ]
                [ Select.from ["Point", "Inscribed Circle", "Rectangle Region"] SetNewTrackType 
                , input [ class "form-control", onInput SetNewTrackName] [] 
                , button [ class "ui button", onClick AddTrack ] [ text "New track" ] 
                ]
            ])

drawTracks : Model -> Html Msg
drawTracks model =
    Svg.svg
        [ SvgAt.viewBox ("0 0 " ++ toString model.tracks.width ++ " " ++ toString model.tracks.height)
        , SvgAt.css 
            [ position absolute, top (px 0)
            , left (px 0)
            , Css.maxHeight (px 650)
            , Css.width (pct 100) 
            ]
        ]
        (Tracks.Track.drawTrackList model.currentFrame model.tracks.tracks)
        --[ Svg.circle [ SvgAt.cx xString, SvgAt.cy yString, SvgAt.r "20", SvgAt.fill "red" ] [] ]



pointInfo : Int -> Int -> Point ->  Html Msg
pointInfo currentFrame idx (x, y) =
    let
        toFloatDefault str = String.toFloat str |> Result.withDefault 0
        changeX xStr = UpdateTrackElement currentFrame idx ((toFloatDefault xStr, y) |> PointElement)
        changeY yStr = UpdateTrackElement currentFrame idx ((x, toFloatDefault yStr) |> PointElement)
    in    
        div [ class "ui form", Json.map (HandleEditKeyboardEvent idx) decodeKeyboardEvent |> on "keydown" ]
            [ div [ class "fields" ]
                [ div [ class "field" ] [ input [ type_ "radio", tabindex idx ] [] ]
                , div [ class "field" ]
                    [ label [] [ text "x" ]
                    , input [ onInput changeX, type_ "text", value (toString x) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "y" ]
                    , input [ onInput changeY, type_ "text", value (toString y) ] []
                    ]
                , button [ class "ui button", UpdateTrackElement (currentFrame+1) idx (PointElement (x, y)) |> onClick ] [ text "Copy to next" ]
                , button [ class "ui button", CopyRest (currentFrame+1) idx (PointElement (x, y)) |> onClick ] [ text "Copy to rest" ]
                ]
            ]

inscribedCircleInfo : Int -> Int -> InscribedCircle -> Html Msg
inscribedCircleInfo currentFrame idx ({ cx, cy, width, height, angle } as circ) =
    let
        toFloatDefault str = String.toFloat str |> Result.withDefault 0
        changeCx cxStr = UpdateTrackElement currentFrame idx ({ circ | cx = toFloatDefault cxStr} |> InscribedCircleElement)
        changeCy cyStr = UpdateTrackElement currentFrame idx ({ circ | cy = toFloatDefault cyStr} |> InscribedCircleElement)
        changeWidth  widthStr  = UpdateTrackElement currentFrame idx ({ circ | width = toFloatDefault widthStr} |> InscribedCircleElement)
        changeHeight heightStr = UpdateTrackElement currentFrame idx ({ circ | height = toFloatDefault heightStr} |> InscribedCircleElement)
        changeAngle  angleStr  = UpdateTrackElement currentFrame idx ({ circ | angle = toFloatDefault angleStr} |> InscribedCircleElement)
    in    
        div [ class "ui form", Json.map (HandleEditKeyboardEvent idx) decodeKeyboardEvent |> on "keydown" ]
            [ div [ class "fields" ]
                [ div [ class "field" ]
                    [ label [] [ text "Center X" ]
                    , input [ onInput changeCx, type_ "text", value (toString cx) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "Center Y" ]
                    ,  input [ onInput changeCy, type_ "text", value (toString cy) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "width" ]
                    , input [ onInput changeWidth, type_ "text", value (toString width) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "height" ]
                    , input [ onInput changeHeight, type_ "text", value (toString height) ] [] 
                    ]
                , div [ class "field" ]
                    [ label [] [ text "angle" ]
                    , input [ onInput changeAngle, type_ "text", value (toString angle) ] []            
                    ]
                , div [ class "field" ]
                    [ button 
                        [ class "ui tiny button"
                        , UpdateTrackElement (currentFrame+1) idx (InscribedCircleElement circ) |> onClick 
                        ] 
                        [ text "Copy to next" ]
                    , button 
                        [ class "ui tiny button"
                        , CopyRest (currentFrame+1) idx (InscribedCircleElement circ) |> onClick 
                        ] 
                        [ text "Copy to rest" ]
                    ]
                ]
            ]

rectangleRegionInfo : Int -> Int -> RectangleRegion -> Html Msg
rectangleRegionInfo currentFrame idx ({ x, y, width, height } as rectangle) =
    let
        toFloatDefault str = String.toFloat str |> Result.withDefault 0
        changeX xStr = UpdateTrackElement currentFrame idx ({ rectangle | x = toFloatDefault xStr} |> RectangleRegionElement)
        changeY yStr = UpdateTrackElement currentFrame idx ({ rectangle | y = toFloatDefault yStr} |> RectangleRegionElement)
        changeWidth  widthStr  = UpdateTrackElement currentFrame idx ({ rectangle | width = toFloatDefault widthStr} |> RectangleRegionElement)
        changeHeight heightStr = UpdateTrackElement currentFrame idx ({ rectangle | height = toFloatDefault heightStr} |> RectangleRegionElement)
    in    
        div
            [ class "ui form", Json.map (HandleEditKeyboardEvent idx) decodeKeyboardEvent |> on "keydown" ]
            [ div [ class "fields" ]
                [ div [ class "field" ]
                    [ label [] [ text "x" ]
                    , input [ onInput changeX, type_ "text", value (toString x) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "y" ]
                    , input [ onInput changeY, type_ "text", value (toString y) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "width" ]
                    , input [ onInput changeWidth, type_ "text", value (toString width) ] []
                    ]
                , div [ class "field" ]
                    [ label [] [ text "height" ]
                    , input [ onInput changeHeight, type_ "text", value (toString height) ] []
                    ]
                , button [ class "ui button", UpdateTrackElement (currentFrame+1) idx (RectangleRegionElement rectangle) |> onClick ] [ text "Copy to next" ]
                , button [ class "ui button", CopyRest (currentFrame+1) idx (RectangleRegionElement rectangle) |> onClick ] [ text "Copy to rest" ]
                ]
            ]

trackInfo : Int -> Int -> Track -> Html Msg
trackInfo currentFrame idx track =
    case track.data of
        PointTrack points ->
            case Array.get currentFrame points of
                Just point ->
                    pointInfo currentFrame idx point
                Nothing -> div [] []
        InscribedCircleTrack circles ->
            case Array.get currentFrame circles of
                Just circ ->
                    inscribedCircleInfo currentFrame idx circ
                Nothing -> div [] []
        RectangleRegionTrack rectangles ->
            case Array.get currentFrame rectangles of
                Just rectangle ->
                    rectangleRegionInfo currentFrame idx rectangle
                Nothing -> div [] []
        _ -> div [] []

drawTrackEditingUI : Model -> Html Msg
drawTrackEditingUI model =
    case getListIdx model.trackFocus model.tracks.tracks of
        Just track ->
            div [ class "column"] [ trackInfo model.currentFrame model.trackFocus track ]
        Nothing ->
            div [] []