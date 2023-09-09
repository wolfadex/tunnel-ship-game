module TrackEditor exposing (..)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Events
import Camera3d
import Color
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment3d
import Numeral
import Pixels
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d
import Quantity exposing (Quantity)
import Random
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d exposing (SketchPlane3d)
import Speed exposing (Speed)
import Time
import Update exposing (Update)
import Util.Function
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { keysDown : Set String
    , track : Track
    , camera : EditorCamera
    }


type alias EditorCamera =
    { center : Point3d Meters WorldCoordinates
    , forward : Direction3d WorldCoordinates
    }


type alias Track =
    { shape : Shape WorldCoordinates
    , path : CubicSpline3d.Nondegenerate Meters WorldCoordinates
    , geometry : Scene3d.Entity WorldCoordinates
    }


type WorldCoordinates
    = WorldCoordinates Never


type alias Flags =
    Float


init : Flags -> Update Model Msg
init timeNow =
    let
        initialShape =
            -- Shape.custom
            Shape.newRegular 7

        initialPath =
            CubicSpline3d.fromControlPoints
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 0)
                    (Length.meters 2)
                )
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters -20)
                    (Length.meters 20)
                )
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 20)
                    (Length.meters 20)
                )
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 10)
                    (Length.meters 2)
                )
                |> CubicSpline3d.nondegenerate

        carl a =
            case initialPath of
                Ok p ->
                    p

                Err _ ->
                    carl initialPath

        path =
            carl initialPath

        arcLength =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                path
                |> CubicSpline3d.arcLength
                |> Length.inKilometers
    in
    { keysDown = Set.empty
    , camera =
        let
            center =
                Point3d.meters 10 10 10
        in
        { center = center
        , forward =
            Point3d.origin
                |> Direction3d.from center
                |> Maybe.withDefault Direction3d.positiveX
        }
    , track =
        { shape = initialShape
        , path = path
        , geometry =
            let
                segments =
                    100
            in
            List.range 1 segments
                |> List.map (\i -> viewTunnelRing initialShape path (toFloat i / segments))
                -- DEBUG BELOW
                |> List.append
                    [ List.range 1 100
                        |> List.map
                            (\i ->
                                let
                                    ( p, dir ) =
                                        CubicSpline3d.sample path (toFloat i / segments)
                                in
                                LineSegment3d.from p
                                    (p
                                        |> Point3d.translateIn dir (Length.meters 1)
                                    )
                            )
                        |> Scene3d.Mesh.lineSegments
                        |> Scene3d.mesh (Scene3d.Material.color Color.red)
                    , List.range 1 100
                        |> List.map
                            (\i ->
                                let
                                    ( p, _ ) =
                                        CubicSpline3d.sample path (toFloat i / segments)

                                    dir =
                                        sketchPlaneAt path (toFloat i / segments)
                                            |> SketchPlane3d.yDirection
                                in
                                LineSegment3d.from p
                                    (p
                                        |> Point3d.translateIn dir (Length.meters 1)
                                    )
                            )
                        |> Scene3d.Mesh.lineSegments
                        |> Scene3d.mesh (Scene3d.Material.color Color.green)
                    ]
                |> Scene3d.group
        }
    }
        |> Update.save


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
        [ Browser.Events.onKeyDown decodeKeyDown
        , Browser.Events.onKeyUp decodeKeyUp
        , if Set.member "Alt" model.keysDown then
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
            model
                |> Update.save

        Event timestamp event ->
            model
                |> applyEvent event
                |> Update.save


applyEvent : Event -> Model -> Model
applyEvent event model =
    case event of
        KeyDown key ->
            let
                camera =
                    model.camera
            in
            { model
                | keysDown = Set.insert key model.keysDown
                , camera =
                    { camera
                        | center =
                            let
                                xDir : Direction3d WorldCoordinates
                                xDir =
                                    Viewpoint3d.lookAt
                                        { eyePoint = model.camera.center
                                        , focalPoint =
                                            model.camera.center
                                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                        , upDirection = Direction3d.positiveZ
                                        }
                                        |> Viewpoint3d.xDirection

                                yDir : Direction3d WorldCoordinates
                                yDir =
                                    Viewpoint3d.lookAt
                                        { eyePoint = model.camera.center
                                        , focalPoint =
                                            model.camera.center
                                                |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                        , upDirection = Direction3d.positiveZ
                                        }
                                        |> Viewpoint3d.yDirection
                            in
                            camera.center
                                -- Forward
                                |> Util.Function.applyIf (String.toLower key == "w")
                                    (Point3d.translateIn camera.forward (Length.meters 0.1))
                                -- Backward
                                |> Util.Function.applyIf (String.toLower key == "s")
                                    (Point3d.translateIn camera.forward (Length.meters -0.1))
                                -- Right
                                |> Util.Function.applyIf (String.toLower key == "d")
                                    (Point3d.translateIn xDir (Length.meters 0.1))
                                -- Left
                                |> Util.Function.applyIf (String.toLower key == "a")
                                    (Point3d.translateIn xDir (Length.meters -0.1))
                                -- Up
                                |> Util.Function.applyIf (String.toLower key == "e")
                                    (Point3d.translateIn yDir (Length.meters 0.1))
                                -- Down
                                |> Util.Function.applyIf (String.toLower key == "q")
                                    (Point3d.translateIn yDir (Length.meters -0.1))
                    }
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
                                xAxis : Axis3d Meters WorldCoordinates
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

                                yAxis : Axis3d Meters WorldCoordinates
                                yAxis =
                                    -- Viewpoint3d.lookAt
                                    --     { eyePoint = model.camera.center
                                    --     , focalPoint =
                                    --         model.camera.center
                                    --             |> Point3d.translateIn model.camera.forward (Length.meters 1)
                                    --     , upDirection = Direction3d.positiveZ
                                    --     }
                                    --     |> Viewpoint3d.yDirection
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

        KeyUp key ->
            { model
                | keysDown = Set.remove key model.keysDown
            }


view : Model -> List (Html Msg)
view model =
    [ Scene3d.cloudy
        { dimensions = ( Pixels.int 800, Pixels.int 600 )
        , upDirection = Direction3d.negativeY
        , camera =
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
        , clipDepth = Length.millimeters 0.1
        , background = Scene3d.backgroundColor Color.black
        , entities =
            [ model.track.geometry
            , [ LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveX (Length.meters 1)
                    )
              ]
                |> Scene3d.Mesh.lineSegments
                |> Scene3d.mesh (Scene3d.Material.color Color.red)
            , [ LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveY (Length.meters 1)
                    )
              ]
                |> Scene3d.Mesh.lineSegments
                |> Scene3d.mesh (Scene3d.Material.color Color.green)
            , [ LineSegment3d.from
                    Point3d.origin
                    (Point3d.origin
                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1)
                    )
              ]
                |> Scene3d.Mesh.lineSegments
                |> Scene3d.mesh (Scene3d.Material.color Color.blue)
            ]
        }

    -- DEBUG
    , Html.div
        []
        [ Html.span [ Html.Attributes.class "score" ]
            [ ((model.track.path
                    |> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                    |> CubicSpline3d.arcLength
                    |> Length.inKilometers
                    |> Numeral.format "0,0.00"
               )
                ++ " km"
              )
                |> Html.text
            ]
        ]
    ]
