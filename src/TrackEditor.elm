module TrackEditor exposing (..)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis2d
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Coordinates
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import DebugFlags exposing (DebugFlags)
import Direction2d
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame2d exposing (Frame2d)
import Frame3d
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Numeral
import Pixels exposing (Pixels)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Polygon2d
import Quantity exposing (Quantity)
import Random
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d exposing (SketchPlane3d)
import Speed exposing (Speed)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time
import Track exposing (Track)
import Update exposing (Update)
import Util.Debug
import Util.Function
import Vector2d
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import Visible exposing (Visible)


type alias Model =
    { keysDown : Set String
    , modifiersDown : Modifiers
    , lastTickTime : Time.Posix
    , track : Track
    , camera : EditorCamera
    , debugFlags : DebugFlags
    , movingControlPoint : Maybe Track.ActiveControlPoint
    }


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


init : Flags -> Update Model Msg
init timeNow =
    let
        initialShape : Shape Coordinates.World
        initialShape =
            -- Shape.custom
            Shape.newRegular 5

        initialPath =
            CubicSpline3d.fromControlPoints
                (Point3d.xyz
                    (Length.meters 1)
                    (Length.meters 1)
                    (Length.meters 0)
                )
                (Point3d.xyz
                    (Length.meters 5)
                    (Length.meters 20)
                    (Length.meters 0)
                )
                (Point3d.xyz
                    (Length.meters 9)
                    (Length.meters -8)
                    (Length.meters 0)
                )
                (Point3d.xyz
                    (Length.meters 13)
                    (Length.meters 8)
                    (Length.meters 0)
                )

        path : CubicSpline3d.Nondegenerate Meters Coordinates.World
        path =
            carlPath initialPath

        debugFlags : DebugFlags
        debugFlags =
            { tunnelVisible = Visible.Visible
            , trackPathVisible = Visible.Visible
            , trackPathDownDirectionVisible = Visible.Visible
            }
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
    , track = Track.init initialShape debugFlags
    , debugFlags = debugFlags
    , movingControlPoint = Nothing
    , lastTickTime = Time.millisToPosix (round timeNow)
    }
        |> Update.save


carlPath : CubicSpline3d Meters Coordinates.World -> CubicSpline3d.Nondegenerate Meters Coordinates.World
carlPath a =
    case CubicSpline3d.nondegenerate a of
        Ok p ->
            p

        Err err ->
            let
                _ =
                    Debug.log "Error in carlPath" err
            in
            carlPath a


createTrackPathGeometry : CubicSpline3d.Nondegenerate Meters coordinates -> Int -> Float -> Scene3d.Entity coordinates
createTrackPathGeometry path segmentsInt segmentsFloat =
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                let
                    ( p, dir ) =
                        CubicSpline3d.sample path (toFloat i / segmentsFloat)
                in
                LineSegment3d.from p
                    (p
                        |> Point3d.translateIn dir (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.red)


createTrackDownGeometry : CubicSpline3d.Nondegenerate Meters coordinates -> Int -> Float -> Scene3d.Entity coordinates
createTrackDownGeometry path segmentsInt segmentsFloat =
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                let
                    ( p, _ ) =
                        CubicSpline3d.sample path (toFloat i / segmentsFloat)

                    dir =
                        sketchPlaneAt path (toFloat i / segmentsFloat)
                            |> SketchPlane3d.yDirection
                in
                LineSegment3d.from p
                    (p
                        |> Point3d.translateIn dir (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.green)


sketchPlaneAt : CubicSpline3d.Nondegenerate Meters coordinates -> Float -> SketchPlane3d Meters coordinates defines
sketchPlaneAt path dist =
    let
        ( center, normal ) =
            CubicSpline3d.sample path dist
    in
    SketchPlane3d.through center normal


viewTunnelRing : Shape coordinates -> CubicSpline3d.Nondegenerate Meters coordinates -> Float -> Scene3d.Entity coordinates
viewTunnelRing shape path dist =
    shape
        |> Polygon2d.edges
        |> List.map (LineSegment3d.on (sketchPlaneAt path dist))
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.lightPurple)


viewTunnelVertConnectors : Point2d Meters coordinates -> Scene3d.Entity coordinates
viewTunnelVertConnectors point =
    let
        point3d : Point3d Meters coordinates
        point3d =
            Point3d.on
                SketchPlane3d.xz
                point
    in
    point3d
        |> Point3d.translateIn Direction3d.positiveY (Length.kilometers 100)
        |> LineSegment3d.from
            (point3d
                |> Point3d.translateIn Direction3d.positiveY (Length.meters -50)
            )
        |> List.singleton
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh
            (Scene3d.Material.color Color.lightPurple)


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
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map KeyDown
        |> decodeEvent


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "movementX" Json.Decode.float)
        (Json.Decode.field "movementY" Json.Decode.float)
        |> decodeEvent


decodeKeyUp : Json.Decode.Decoder Msg
decodeKeyUp =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map KeyUp
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


type Event
    = KeyDown String
    | KeyUp String
    | MouseMove Float Float


type Direction
    = Clockwise
    | CounterClockwise


update : Msg -> Model -> Update Model Msg
update msg model =
    case msg of
        AnimationFrame timestamp ->
            { model | lastTickTime = timestamp }
                |> tick (timeDelta model timestamp)

        Event timestamp event ->
            model
                |> applyEvent event
                |> tick (timeDelta model timestamp)

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
                        in
                        { model
                            | movingControlPoint = Just newMovingControlPoint
                            , track =
                                Track.moveControlPoint
                                    { debugFlags = model.debugFlags
                                    , movingControlPoint = newMovingControlPoint
                                    , camera = camera
                                    , screenRectangle = screenRectangle
                                    }
                                    model.track
                        }
                            |> Update.save

                    else
                        model
                            |> Update.save

        PointerUp index ->
            { model | movingControlPoint = Nothing }
                |> Update.save


redrawTrackGeometry : Model -> Model
redrawTrackGeometry model =
    { model
        | track = Track.newDebugFlags model.debugFlags model.track
    }


timeDelta : Model -> Time.Posix -> Duration
timeDelta model timestamp =
    (Time.posixToMillis timestamp - Time.posixToMillis model.lastTickTime)
        |> toFloat
        |> Duration.milliseconds


tick : Duration -> Model -> Update Model Msg
tick deltaTime model =
    model
        |> Update.save
        |> moveCamera deltaTime


moveCamera : Duration -> Update Model Msg -> Update Model Msg
moveCamera deltaTime =
    Update.mapModel
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
                                |> Util.Function.applyIf (Set.member "W" model.keysDown)
                                    (Point3d.translateIn camera.forward distance)
                                -- Backward
                                |> Util.Function.applyIf (Set.member "S" model.keysDown)
                                    (Point3d.translateIn camera.forward (Quantity.negate distance))
                                -- Right
                                |> Util.Function.applyIf (Set.member "D" model.keysDown)
                                    (Point3d.translateIn xDir distance)
                                -- Left
                                |> Util.Function.applyIf (Set.member "A" model.keysDown)
                                    (Point3d.translateIn xDir (Quantity.negate distance))
                                -- Up
                                |> Util.Function.applyIf (Set.member "E" model.keysDown)
                                    (Point3d.translateIn yDir distance)
                                -- Down
                                |> Util.Function.applyIf (Set.member "Q" model.keysDown)
                                    (Point3d.translateIn yDir (Quantity.negate distance))
                    }
            }
        )


applyEvent : Event -> Model -> Model
applyEvent event model =
    case event of
        KeyDown key ->
            let
                modifiersDown =
                    model.modifiersDown
            in
            { model
                | keysDown = Set.insert (String.toUpper key) model.keysDown
                , modifiersDown =
                    if key == "Shift" then
                        { modifiersDown | shift = True }

                    else if key == "Control" then
                        { modifiersDown | ctrl = True }

                    else if key == "Alt" then
                        { modifiersDown | alt = True }

                    else
                        modifiersDown
            }

        KeyUp key ->
            let
                modifiersDown =
                    model.modifiersDown
            in
            { model
                | keysDown = Set.remove (String.toUpper key) model.keysDown
                , modifiersDown =
                    if key == "Shift" then
                        { modifiersDown | shift = False }

                    else if key == "Control" then
                        { modifiersDown | ctrl = False }

                    else if key == "Alt" then
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
    [ Scene3d.cloudy
        { dimensions = ( Pixels.int viewSize.width, Pixels.int viewSize.height )
        , upDirection = Direction3d.negativeY
        , camera = camera
        , clipDepth = Length.millimeters 0.1
        , background = Scene3d.backgroundColor Color.black
        , entities =
            [ Track.view model.track
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

    -- DEBUG
    , Html.div
        []
        [ Html.span [ Html.Attributes.class "score" ]
            [ ((Track.length model.track
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
        ]
    , Track.viewControlPoints
        { viewSize = viewSize
        , camera = camera
        , movingControlPoint = model.movingControlPoint
        , onPointerDown = PointerDown
        , onPointerMove = PointerMove
        , onPointerUp = PointerUp
        }
        model.track
    ]
