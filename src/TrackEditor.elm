port module TrackEditor exposing
    ( Direction(..)
    , EditorCamera
    , Effect(..)
    , Event(..)
    , Flags
    , Model
    , Modifiers
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Angle
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Coordinates
import DebugFlags exposing (DebugFlags)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment3d
import Numeral
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import Set exposing (Set)
import Shape exposing (Shape)
import Speed
import Time
import Track exposing (Track)
import Update exposing (Update)
import Util.Function
import Viewpoint3d
import Visible exposing (Visible)


type alias Model =
    { keysDown : Set String
    , modifiersDown : Modifiers
    , lastTickTime : Time.Posix
    , potentialTrack : Track.Potential
    , trackPreview : Maybe Track.Track
    , previewShipDistance : Length
    , previewShipGeometry : Block3d Meters Coordinates.World
    , camera : EditorCamera
    , debugFlags : DebugFlags
    , movingControlPoint : Maybe Track.ActiveControlPoint
    , editMode : EditMode
    }


type EditMode
    = MoveGlobal
    | MoveLocal
    | RotateLocal


type alias Modifiers =
    { shift : Bool
    , ctrl : Bool
    , alt : Bool
    }


type alias EditorCamera =
    { center : Point3d Meters Coordinates.World
    , forward : Direction3d Coordinates.World
    }


type alias Flags =
    Float


init : Flags -> Update Model Msg Effect
init timeNow =
    let
        initialShape : Shape Coordinates.Flat
        initialShape =
            -- Shape.custom
            Shape.newRegular 5

        debugFlags : DebugFlags
        debugFlags =
            { tunnelVisible = Visible.Visible
            , trackPathVisible = Visible.Visible
            , trackPathDownDirectionVisible = Visible.Visible
            }

        potentialTrack : Track.Potential
        potentialTrack =
            Track.init initialShape (Just debugFlags)
    in
    { keysDown = Set.empty
    , modifiersDown = { shift = False, ctrl = False, alt = False }
    , camera =
        let
            center =
                Point3d.meters -6 -6 10
        in
        { center = center
        , forward =
            Point3d.meters 4 4 0
                |> Direction3d.from center
                |> Maybe.withDefault Direction3d.positiveX
        }
    , potentialTrack = Track.init initialShape (Just debugFlags)
    , trackPreview =
        potentialTrack
            |> Track.fromPotential
            |> Result.toMaybe
    , previewShipDistance = Length.meters 0
    , previewShipGeometry =
        Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 0.5
            , Length.meters 1
            , Length.meters 0.125
            )
    , debugFlags = debugFlags
    , movingControlPoint = Nothing
    , editMode = MoveGlobal
    , lastTickTime = Time.millisToPosix (round timeNow)
    }
        |> Update.save


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onKeyDown decodeKeyDown
        , Browser.Events.onKeyUp decodeKeyUp
        , if model.modifiersDown.alt then
            Browser.Events.onMouseMove decodeMouseMove

          else
            Sub.none
        ]


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map2 KeyDown
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "code" Json.Decode.string)
        |> decodeEvent


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "movementX" Json.Decode.float)
        (Json.Decode.field "movementY" Json.Decode.float)
        |> decodeEvent


decodeKeyUp : Json.Decode.Decoder Msg
decodeKeyUp =
    Json.Decode.map2 KeyUp
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "code" Json.Decode.string)
        |> decodeEvent


decodeEvent : Json.Decode.Decoder Event -> Json.Decode.Decoder Msg
decodeEvent decoder =
    Json.Decode.map2 Event
        (Json.Decode.field "___timeStamp" Json.Decode.int
            |> Json.Decode.map Time.millisToPosix
        )
        decoder


type Msg
    = AnimationFrame Time.Posix
    | Event Time.Posix Event
    | DebugTrackPathToggled Visible
    | DebugTrackPathDownDirectionToggled Visible
    | DebugTunnelToggled Visible
    | PointerDown Track.ActiveControlPoint
    | PointerMove Int (Point2d Pixels Coordinates.Screen)
    | PointerUp Int
    | TestTrackClicked
    | PreviewShipDistanceChanged String
    | EditModeChangeClicked EditMode


type Effect
    = TestTrack Track.Track


type Event
    = KeyDown String String
    | KeyUp String String
    | MouseMove Float Float


type Direction
    = CounterClockwise


update : Msg -> Model -> Update Model Msg Effect
update msg model =
    case msg of
        AnimationFrame timestamp ->
            { model | lastTickTime = timestamp }
                |> tick (timeDelta model timestamp)

        Event timestamp event ->
            model
                |> applyEvent event
                |> tick (timeDelta model timestamp)

        TestTrackClicked ->
            case Track.fromPotential model.potentialTrack of
                Err err ->
                    Debug.todo ""

                Ok track ->
                    model
                        |> Update.save

        -- |> Update.withEffect (TestTrack track)
        DebugTrackPathToggled visible ->
            let
                debugFlags =
                    model.debugFlags
            in
            { model
                | debugFlags =
                    { debugFlags
                        | trackPathVisible = visible
                    }
            }
                |> redrawTrackGeometry
                |> Update.save

        DebugTrackPathDownDirectionToggled visible ->
            let
                debugFlags =
                    model.debugFlags
            in
            { model
                | debugFlags =
                    { debugFlags
                        | trackPathDownDirectionVisible = visible
                    }
            }
                |> redrawTrackGeometry
                |> Update.save

        DebugTunnelToggled visible ->
            let
                debugFlags =
                    model.debugFlags
            in
            { model
                | debugFlags =
                    { debugFlags
                        | tunnelVisible = visible
                    }
            }
                |> redrawTrackGeometry
                |> Update.save

        PreviewShipDistanceChanged str ->
            let
                previewShipDistance =
                    str
                        |> String.toFloat
                        |> Maybe.map Length.kilometers
                        |> Maybe.withDefault model.previewShipDistance
            in
            { model | previewShipDistance = previewShipDistance }
                |> Update.save

        EditModeChangeClicked editMode ->
            { model | editMode = editMode }
                |> Update.save

        PointerDown activeControlPoint ->
            { model
                | movingControlPoint = Just activeControlPoint
            }
                |> Update.save

        PointerMove index point ->
            case model.movingControlPoint of
                Nothing ->
                    model
                        |> Update.save

                Just movingControlPoint ->
                    if index == movingControlPoint.index then
                        let
                            viewSize =
                                { width = 800
                                , height = 600
                                }

                            camera : Camera3d Meters Coordinates.World
                            camera =
                                Camera3d.perspective
                                    { viewpoint =
                                        Viewpoint3d.lookAt
                                            { eyePoint = model.camera.center
                                            , focalPoint =
                                                model.camera.center
                                                    |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                            , upDirection = Direction3d.positiveZ
                                            }
                                    , verticalFieldOfView = Angle.degrees 30
                                    }

                            screenRectangle : Rectangle2d Pixels Coordinates.Screen
                            screenRectangle =
                                Point2d.pixels viewSize.width 0
                                    |> Rectangle2d.from (Point2d.pixels 0 viewSize.height)

                            newMovingControlPoint =
                                { movingControlPoint | point = point }

                            newPotentialTrack : Track.Potential
                            newPotentialTrack =
                                case model.editMode of
                                    MoveGlobal ->
                                        Track.moveControlPoint
                                            { debugFlags = model.debugFlags
                                            , movingControlPoint = newMovingControlPoint
                                            , camera = camera
                                            , screenRectangle = screenRectangle
                                            }
                                            model.potentialTrack

                                    MoveLocal ->
                                        Debug.todo ""

                                    RotateLocal ->
                                        Track.rotateControlPoint
                                            { debugFlags = model.debugFlags
                                            , movingControlPoint = newMovingControlPoint
                                            , camera = camera
                                            , screenRectangle = screenRectangle
                                            }
                                            model.potentialTrack
                        in
                        { model
                            | movingControlPoint = Just newMovingControlPoint
                            , potentialTrack = newPotentialTrack
                            , trackPreview =
                                newPotentialTrack
                                    |> Track.fromPotential
                                    |> Result.toMaybe
                        }
                            |> Update.save

                    else
                        model
                            |> Update.save

        PointerUp _ ->
            { model | movingControlPoint = Nothing }
                |> Update.save


redrawTrackGeometry : Model -> Model
redrawTrackGeometry model =
    { model
        | potentialTrack = Track.newDebugFlags model.debugFlags model.potentialTrack
    }


timeDelta : Model -> Time.Posix -> Duration
timeDelta model timestamp =
    (Time.posixToMillis timestamp - Time.posixToMillis model.lastTickTime)
        |> toFloat
        |> Duration.milliseconds


tick : Duration -> Model -> Update Model Msg Effect
tick deltaTime model =
    model
        |> Update.save
        |> moveCamera deltaTime
        |> movePreviewShip deltaTime


movePreviewShip : Duration -> Update Model Msg Effect -> Update Model Msg Effect
movePreviewShip deltaTime =
    Update.mapModel
        (\model ->
            case model.trackPreview of
                Nothing ->
                    model

                Just trackPreview ->
                    let
                        previewShipDistance =
                            Speed.kilometersPerHour 5
                                |> Quantity.for deltaTime
                                |> Quantity.plus model.previewShipDistance

                        lengthOfTrack =
                            Track.length trackPreview
                    in
                    { model
                        | previewShipDistance =
                            if previewShipDistance |> Quantity.greaterThan lengthOfTrack then
                                Length.kilometers 0

                            else
                                previewShipDistance
                    }
        )


moveCamera : Duration -> Update Model Msg Effect -> Update Model Msg Effect
moveCamera deltaTime =
    Update.andThen
        (\model ->
            let
                camera =
                    model.camera
            in
            { model
                | camera =
                    { camera
                        | center =
                            let
                                xDir : Direction3d Coordinates.World
                                xDir =
                                    Viewpoint3d.lookAt
                                        { eyePoint = model.camera.center
                                        , focalPoint =
                                            model.camera.center
                                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                        , upDirection = Direction3d.positiveZ
                                        }
                                        |> Viewpoint3d.xDirection

                                yDir : Direction3d Coordinates.World
                                yDir =
                                    Viewpoint3d.lookAt
                                        { eyePoint = model.camera.center
                                        , focalPoint =
                                            model.camera.center
                                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                        , upDirection = Direction3d.positiveZ
                                        }
                                        |> Viewpoint3d.yDirection

                                distance : Length
                                distance =
                                    Speed.metersPerSecond 5
                                        |> Quantity.for deltaTime
                                        |> Util.Function.applyIf model.modifiersDown.shift
                                            (Quantity.multiplyBy 2)
                            in
                            camera.center
                                -- Forward
                                |> Util.Function.applyIf (Set.member "KeyW" model.keysDown)
                                    (Point3d.translateIn camera.forward distance)
                                -- Backward
                                |> Util.Function.applyIf (Set.member "KeyS" model.keysDown)
                                    (Point3d.translateIn camera.forward (Quantity.negate distance))
                                -- Right
                                |> Util.Function.applyIf (Set.member "KeyD" model.keysDown)
                                    (Point3d.translateIn xDir distance)
                                -- Left
                                |> Util.Function.applyIf (Set.member "KeyA" model.keysDown)
                                    (Point3d.translateIn xDir (Quantity.negate distance))
                                -- Up
                                |> Util.Function.applyIf (Set.member "KeyE" model.keysDown)
                                    (Point3d.translateIn yDir distance)
                                -- Down
                                |> Util.Function.applyIf (Set.member "KeyQ" model.keysDown)
                                    (Point3d.translateIn yDir (Quantity.negate distance))
                    }
            }
                |> Update.save
         -- |> Update.withCmd (lockMouseToCanvas 5)
        )


port lockMouseToCanvas : Bool -> Cmd msg


applyEvent : Event -> Model -> Model
applyEvent event model =
    case event of
        KeyDown _ keyCode ->
            let
                modifiersDown =
                    model.modifiersDown
            in
            { model
                | keysDown = Set.insert keyCode model.keysDown
                , modifiersDown =
                    if keyCode == "ShiftLeft" || keyCode == "ShiftRight" then
                        { modifiersDown | shift = True }

                    else if keyCode == "ControlLeft" || keyCode == "ControlRight" then
                        { modifiersDown | ctrl = True }

                    else if keyCode == "AltLeft" || keyCode == "AltRight" then
                        { modifiersDown | alt = True }

                    else
                        modifiersDown
            }

        KeyUp _ keyCode ->
            let
                modifiersDown =
                    model.modifiersDown
            in
            { model
                | keysDown = Set.remove keyCode model.keysDown
                , modifiersDown =
                    if keyCode == "ShiftLeft" || keyCode == "ShiftRight" then
                        { modifiersDown | shift = False }

                    else if keyCode == "ControlLeft" || keyCode == "ControlRight" then
                        { modifiersDown | ctrl = False }

                    else if keyCode == "AltLeft" || keyCode == "AltRight" then
                        { modifiersDown | alt = False }

                    else
                        modifiersDown
            }

        MouseMove x y ->
            let
                camera =
                    model.camera
            in
            { model
                | camera =
                    { camera
                        | forward =
                            let
                                xAxis : Axis3d Meters Coordinates.World
                                xAxis =
                                    Viewpoint3d.lookAt
                                        { eyePoint = model.camera.center
                                        , focalPoint =
                                            model.camera.center
                                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                        , upDirection = Direction3d.positiveZ
                                        }
                                        |> Viewpoint3d.xDirection
                                        |> Axis3d.through model.camera.center

                                yAxis : Axis3d Meters Coordinates.World
                                yAxis =
                                    Direction3d.positiveZ
                                        |> Axis3d.through model.camera.center
                            in
                            camera.forward
                                -- Pitch
                                |> Direction3d.rotateAround xAxis (Angle.degrees (-y / 10))
                                -- Yaw
                                |> Direction3d.rotateAround yAxis (Angle.degrees (-x / 10))
                    }
            }


view : Model -> List (Html Msg)
view model =
    let
        viewSize =
            { width = 800
            , height = 600
            }

        camera : Camera3d Meters Coordinates.World
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = model.camera.center
                        , focalPoint =
                            model.camera.center
                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    [ viewTrackEditorCanvas viewSize camera model
    , Html.div [ Html.Attributes.style "display" "flex" ]
        [ Html.div [ Html.Attributes.style "flex" "1" ]
            [ Html.div
                []
                [ Html.span [ Html.Attributes.class "score" ]
                    [ ((case Track.potentialLength model.potentialTrack of
                            Err (Track.NotNondegenerate _) ->
                                "N/A"

                            Ok length ->
                                length
                                    |> Length.inKilometers
                                    |> Numeral.format "0,0.000"
                       )
                        ++ " km"
                      )
                        |> Html.text
                    ]
                ]
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                ]
                [ Visible.view
                    { label = "Track path visible"
                    , value = model.debugFlags.trackPathVisible
                    , onChange = DebugTrackPathToggled
                    }
                , Visible.view
                    { label = "Track path down direction visible"
                    , value = model.debugFlags.trackPathDownDirectionVisible
                    , onChange = DebugTrackPathDownDirectionToggled
                    }
                , Visible.view
                    { label = "Tunnel visible"
                    , value = model.debugFlags.tunnelVisible
                    , onChange = DebugTunnelToggled
                    }
                , Html.label []
                    [ Html.text "Preview ship distance: "
                    , Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max
                            (model.potentialTrack
                                |> Track.potentialLength
                                |> Result.withDefault (Length.meters 0)
                                |> Length.inKilometers
                                |> String.fromFloat
                            )
                        , Html.Attributes.value
                            (model.previewShipDistance
                                |> Length.inKilometers
                                |> String.fromFloat
                            )
                        , Html.Attributes.step
                            (Length.meter
                                |> Length.inKilometers
                                |> String.fromFloat
                            )
                        , Html.Events.onInput PreviewShipDistanceChanged
                        ]
                        []
                    ]
                , Html.div []
                    [ Html.button
                        [ Html.Events.onClick (EditModeChangeClicked MoveGlobal)
                        , Html.Attributes.attribute "aria-pressed" <|
                            case model.editMode of
                                MoveGlobal ->
                                    "true"

                                _ ->
                                    "false"
                        ]
                        [ Html.text "Move Global" ]
                    , Html.button
                        [ Html.Events.onClick (EditModeChangeClicked MoveLocal)
                        , Html.Attributes.attribute "aria-pressed" <|
                            case model.editMode of
                                MoveLocal ->
                                    "true"

                                _ ->
                                    "false"
                        ]
                        [ Html.text "Move Local" ]
                    , Html.button
                        [ Html.Attributes.disabled True ]
                        [ Html.text "Rotate Global" ]
                    , Html.button
                        [ Html.Events.onClick (EditModeChangeClicked RotateLocal)
                        , Html.Attributes.attribute "aria-pressed" <|
                            case model.editMode of
                                RotateLocal ->
                                    "true"

                                _ ->
                                    "false"
                        ]
                        [ Html.text "Rotate Local" ]
                    ]
                ]
            , Track.viewControlPoints
                { viewSize = viewSize
                , camera = camera
                , movingControlPoint = model.movingControlPoint
                , onPointerDown = PointerDown
                , onPointerMove = PointerMove
                , onPointerUp = PointerUp
                }
                model.potentialTrack
            ]
        , viewTrackPreview viewSize model

        -- , Html.button
        --     [ Html.Events.onClick TestTrackClicked ]
        --     [ Html.text "Test track" ]
        ]
    ]


viewTrackEditorCanvas : { width : Int, height : Int } -> Camera3d Meters Coordinates.World -> Model -> Html Msg
viewTrackEditorCanvas viewSize camera model =
    Html.div [ Html.Attributes.id "editor-canvas" ]
        [ Scene3d.cloudy
            { dimensions = ( Pixels.int viewSize.width, Pixels.int viewSize.height )
            , upDirection = Direction3d.negativeY
            , camera = camera
            , clipDepth = Length.millimeters 0.1
            , background = Scene3d.backgroundColor Color.black
            , entities =
                [ Track.viewPotential model.potentialTrack
                , LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveX (Length.meters 1)
                    )
                    |> Scene3d.lineSegment (Scene3d.Material.color Color.red)
                , LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveY (Length.meters 1)
                    )
                    |> Scene3d.lineSegment (Scene3d.Material.color Color.green)
                , LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1)
                    )
                    |> Scene3d.lineSegment (Scene3d.Material.color Color.blue)
                ]
            }
        ]


viewTrackPreview : { width : Int, height : Int } -> Model -> Html Msg
viewTrackPreview viewSize model =
    case model.trackPreview of
        Nothing ->
            Html.text "Invalid track"

        Just trackPreview ->
            let
                focalPoint =
                    Track.sample trackPreview model.previewShipDistance
                        |> Frame3d.originPoint

                frame =
                    Track.sample
                        trackPreview
                        (model.previewShipDistance
                            |> Quantity.minus (Length.meters 4)
                        )

                followPoint =
                    Frame3d.originPoint frame

                upDir =
                    frame
                        |> Frame3d.zDirection
            in
            Scene3d.cloudy
                { dimensions = ( Pixels.int (viewSize.width // 2), Pixels.int (viewSize.height // 2) )
                , upDirection = Direction3d.negativeY
                , camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { eyePoint = followPoint
                                , focalPoint = focalPoint
                                , upDirection = upDir
                                }
                        , verticalFieldOfView = Angle.degrees 30
                        }
                , clipDepth = Length.millimeters 0.1
                , background = Scene3d.backgroundColor Color.black
                , entities =
                    [ Track.view trackPreview
                    , viewShipPreview focalPoint trackPreview model.previewShipDistance model.previewShipGeometry
                    ]
                }


viewShipPreview : Point3d Meters Coordinates.World -> Track -> Length -> Block3d Meters Coordinates.World -> Scene3d.Entity Coordinates.World
viewShipPreview focalPoint track distance geometry =
    let
        frame =
            Track.sample track distance

        center =
            Frame3d.originPoint frame

        shipFinal =
            geometry
                |> Block3d.placeIn frame
                |> Block3d.translateIn (Frame3d.zDirection frame) (Length.meters -0.75)
    in
    Scene3d.group
        [ shipFinal
            |> Scene3d.block (Scene3d.Material.matte Color.green)
        , LineSegment3d.from
            (Block3d.centerPoint shipFinal)
            (shipFinal
                |> Block3d.centerPoint
                |> Point3d.translateIn (Frame3d.xDirection frame) (Length.meters 1)
            )
            |> Scene3d.lineSegment (Scene3d.Material.color Color.red)
        ]
