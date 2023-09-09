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
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
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
import LineSegment2d
import LineSegment3d
import Numeral
import Pixels exposing (Pixels)
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
import Svg
import Svg.Attributes
import Time
import Update exposing (Update)
import Util.Function
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { keysDown : Set String
    , track : Track
    , camera : EditorCamera
    , debugFlags : DebugFlags
    }


type alias DebugFlags =
    { tunnelVisible : Visible
    , trackPathVisible : Visible
    , trackPathDownDirectionVisible : Visible
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


type Visible
    = Visible
    | Hidden


type alias Flags =
    Float


init : Flags -> Update Model Msg
init timeNow =
    let
        initialShape : Shape WorldCoordinates
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
                |> CubicSpline3d.nondegenerate

        carl a =
            case initialPath of
                Ok p ->
                    p

                Err _ ->
                    carl initialPath

        path : CubicSpline3d.Nondegenerate Meters WorldCoordinates
        path =
            carl initialPath

        debugFlags : DebugFlags
        debugFlags =
            { tunnelVisible = Visible
            , trackPathVisible = Visible
            , trackPathDownDirectionVisible = Visible
            }
    in
    { keysDown = Set.empty
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
    , track =
        { shape = initialShape
        , path = path
        , geometry = createTrackGeometry debugFlags initialShape path
        }
    , debugFlags = debugFlags
    }
        |> Update.save


createTrackGeometry : DebugFlags -> Shape coordinates -> CubicSpline3d.Nondegenerate Meters coordinates -> Scene3d.Entity coordinates
createTrackGeometry debugFlags initialShape path =
    let
        arcLength : Float
        arcLength =
            path
                |> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                |> CubicSpline3d.arcLength
                |> Length.inKilometers

        segmentsFloat : Float
        segmentsFloat =
            arcLength * 1500

        segmentsInt : Int
        segmentsInt =
            round segmentsFloat
    in
    [ viewIfVisible debugFlags.trackPathVisible
        [ createTrackPathGeometry path segmentsInt segmentsFloat ]
    , viewIfVisible debugFlags.trackPathDownDirectionVisible
        [ createTrackDownGeometry path segmentsInt segmentsFloat ]
    , List.range 0 segmentsInt
        |> List.map (\i -> viewTunnelRing initialShape path (toFloat i / segmentsFloat))
        |> viewIfVisible debugFlags.tunnelVisible
    ]
        |> List.concatMap identity
        |> Scene3d.group


viewIfVisible : Visible -> List (Scene3d.Entity coordinates) -> List (Scene3d.Entity coordinates)
viewIfVisible visible entities =
    case visible of
        Visible ->
            entities

        Hidden ->
            []


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
    | DebugTrackPathToggled Visible
    | DebugTrackPathDownDirectionToggled Visible
    | DebugTunnelToggled Visible


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


redrawTrackGeometry : Model -> Model
redrawTrackGeometry model =
    let
        track =
            model.track
    in
    { model
        | track =
            { track
                | geometry = createTrackGeometry model.debugFlags track.shape track.path
            }
    }


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
    let
        camera : Camera3d Meters WorldCoordinates
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
        { dimensions = ( Pixels.int 800, Pixels.int 600 )
        , upDirection = Direction3d.negativeY
        , camera = camera
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
        [ viewVisible
            { label = "Track path visible"
            , value = model.debugFlags.trackPathVisible
            , onChange = DebugTrackPathToggled
            }
        , viewVisible
            { label = "Track path down direction visible"
            , value = model.debugFlags.trackPathDownDirectionVisible
            , onChange = DebugTrackPathDownDirectionToggled
            }
        , viewVisible
            { label = "Tunnel visible"
            , value = model.debugFlags.tunnelVisible
            , onChange = DebugTunnelToggled
            }
        ]
    , viewControlPoints
        { width = 800
        , height = 600
        }
        camera
        model.track.path
    ]


type ScreenSpace
    = ScreenSpace Never


viewControlPoints : { width : Float, height : Float } -> Camera3d Meters WorldCoordinates -> CubicSpline3d.Nondegenerate Meters WorldCoordinates -> Html Msg
viewControlPoints viewSize camera spline =
    let
        -- Take the 3D model for the logo and rotate it by the current angle
        -- rotatedLogo =
        --     blockEntity |> Scene3d.rotateAround Axis3d.z angle
        -- Defines the shape of the 'screen' that we will be using when
        --
        -- projecting 3D points into 2D
        screenRectangle : Rectangle2d Pixels.Pixels ScreenSpace
        screenRectangle =
            Point2d.pixels viewSize.width viewSize.height
                |> Rectangle2d.from Point2d.origin

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d : List (Point2d Pixels ScreenSpace)
        vertices2d =
            List.map (Point3d.Projection.toScreenSpace camera screenRectangle)
                [ spline
                    |> CubicSpline3d.fromNondegenerate
                    |> CubicSpline3d.firstControlPoint
                , spline
                    |> CubicSpline3d.fromNondegenerate
                    |> CubicSpline3d.secondControlPoint
                , spline
                    |> CubicSpline3d.fromNondegenerate
                    |> CubicSpline3d.thirdControlPoint
                , spline
                    |> CubicSpline3d.fromNondegenerate
                    |> CubicSpline3d.fourthControlPoint
                ]

        controlPoints : Svg.Svg msg
        controlPoints =
            List.map
                (\vertex ->
                    Svg.g
                        [ Svg.Attributes.class "galactic-label-focus-civ"
                        ]
                        [ Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(200, 255, 200)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"

                            -- TODO
                            -- , Svg.Events.onClick (onPressSolarSystem solarSystemId)
                            , Html.Attributes.attribute "tabindex" "0"
                            ]
                            (Circle2d.withRadius (Pixels.float 16) vertex)
                        ]
                )
                vertices2d
                |> Svg.g []
                |> Geometry.Svg.relativeTo topLeftFrame

        segmentsSvgs : Svg.Svg msg
        segmentsSvgs =
            List.foldl
                (\controlPoint acc ->
                    case acc of
                        Nothing ->
                            Just ( controlPoint, [] )

                        Just ( previousControlPoint, segments ) ->
                            Just
                                ( controlPoint
                                , Geometry.Svg.lineSegment2d
                                    [ Svg.Attributes.stroke "red"
                                    , Svg.Attributes.strokeWidth "0.5"
                                    , Svg.Attributes.strokeDasharray "5 5"
                                    , Svg.Attributes.class "galactic-label-ignore"
                                    ]
                                    (LineSegment2d.from previousControlPoint controlPoint)
                                    :: segments
                                )
                )
                Nothing
                vertices2d
                |> Maybe.map Tuple.second
                |> Maybe.withDefault []
                |> Svg.g []
                |> Geometry.Svg.relativeTo topLeftFrame

        -- Used for converting from coordinates relative to the bottom-left
        -- corner of the 2D drawing into coordinates relative to the top-left
        -- corner (which is what SVG natively works in)
        topLeftFrame : Frame2d.Frame2d Pixels.Pixels coordinates defines2
        topLeftFrame =
            Frame2d.reverseY (Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float viewSize.height)))
    in
    -- Create an SVG element with the projected points, lines and associated labels
    Svg.svg
        [ Html.Attributes.width (floor viewSize.width)
        , Html.Attributes.height (floor viewSize.height)
        , Svg.Attributes.class "track-editor-svg"
        ]
        [ controlPoints
        , segmentsSvgs
        ]


viewVisible : { label : String, onChange : Visible -> Msg, value : Visible } -> Html Msg
viewVisible { onChange, value, label } =
    Html.label []
        [ Html.text label
        , Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked <|
                case value of
                    Visible ->
                        True

                    Hidden ->
                        False
            , Html.Events.onCheck
                (\isChecked ->
                    onChange <|
                        if isChecked then
                            Visible

                        else
                            Hidden
                )
            ]
            []
        ]
