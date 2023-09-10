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
import Update exposing (Update)
import Util.Debug
import Util.Function
import Vector2d
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { keysDown : Set String
    , track : Track
    , camera : EditorCamera
    , debugFlags : DebugFlags
    , movingControlPoint : Maybe ActiveControlPoint
    }


type alias ActiveControlPoint =
    { pointerId : Json.Decode.Value
    , index : Int
    , point : Point2d Pixels ScreenSpace
    , direction : Direction3d WorldCoordinates
    , plane : Plane3d Meters WorldCoordinates
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

        path : CubicSpline3d.Nondegenerate Meters WorldCoordinates
        path =
            carlPath initialPath

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
    , movingControlPoint = Nothing
    }
        |> Update.save


carlPath : CubicSpline3d Meters WorldCoordinates -> CubicSpline3d.Nondegenerate Meters WorldCoordinates
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
    | PointerDown ActiveControlPoint
    | PointerMove Int (Point2d Pixels ScreenSpace)
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

                            screenRectangle : Rectangle2d Pixels ScreenSpace
                            screenRectangle =
                                Point2d.pixels viewSize.width 0
                                    |> Rectangle2d.from (Point2d.pixels 0 viewSize.height)

                            newControlPoint : Int -> Point3d Meters WorldCoordinates
                            newControlPoint i =
                                let
                                    oldControlPoint =
                                        model.track.path
                                            |> CubicSpline3d.fromNondegenerate
                                            |> (case i of
                                                    0 ->
                                                        CubicSpline3d.firstControlPoint

                                                    1 ->
                                                        CubicSpline3d.secondControlPoint

                                                    2 ->
                                                        CubicSpline3d.thirdControlPoint

                                                    3 ->
                                                        CubicSpline3d.fourthControlPoint

                                                    _ ->
                                                        let
                                                            _ =
                                                                Debug.log "Invalid control point index" i
                                                        in
                                                        CubicSpline3d.firstControlPoint
                                               )

                                    axis =
                                        Axis3d.through oldControlPoint movingControlPoint.direction
                                in
                                if movingControlPoint.index == i then
                                    point
                                        |> Camera3d.ray camera screenRectangle
                                        |> Axis3d.intersectionWithPlane movingControlPoint.plane
                                        |> Maybe.map (Point3d.projectOntoAxis axis)
                                        |> Maybe.withDefault oldControlPoint

                                else
                                    oldControlPoint

                            track =
                                model.track

                            newPath =
                                carlPath
                                    (CubicSpline3d.fromControlPoints
                                        (newControlPoint 0)
                                        (newControlPoint 1)
                                        (newControlPoint 2)
                                        (newControlPoint 3)
                                    )
                        in
                        { model
                            | movingControlPoint =
                                Just
                                    { movingControlPoint
                                        | point = point
                                    }
                            , track =
                                { track
                                    | path = newPath
                                    , geometry = createTrackGeometry model.debugFlags track.shape newPath
                                }
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
        viewSize =
            { width = 800
            , height = 600
            }

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
        { dimensions = ( Pixels.int viewSize.width, Pixels.int viewSize.height )
        , upDirection = Direction3d.negativeY
        , camera = camera
        , clipDepth = Length.millimeters 0.1
        , background = Scene3d.backgroundColor Color.black
        , entities =
            [ model.track.geometry
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
        viewSize
        camera
        model.movingControlPoint
        model.track.path
    ]


type ScreenSpace
    = ScreenSpace Never


viewControlPoints :
    { width : Float, height : Float }
    -> Camera3d Meters WorldCoordinates
    -> Maybe ActiveControlPoint
    -> CubicSpline3d.Nondegenerate Meters WorldCoordinates
    -> Html Msg
viewControlPoints viewSize camera movingControlPoint spline =
    let
        -- Take the 3D model for the logo and rotate it by the current angle
        -- rotatedLogo =
        --     blockEntity |> Scene3d.rotateAround Axis3d.z angle
        -- Defines the shape of the 'screen' that we will be using when
        --
        -- projecting 3D points into 2D
        screenRectangle : Rectangle2d Pixels ScreenSpace
        screenRectangle =
            Point2d.pixels viewSize.width viewSize.height
                |> Rectangle2d.from Point2d.origin

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        controlPoints :
            List
                { center : Point2d Pixels ScreenSpace
                , point : Point3d Meters WorldCoordinates
                , xSegment : LineSegment2d Pixels ScreenSpace
                , ySegment : LineSegment2d Pixels ScreenSpace
                , zSegment : LineSegment2d Pixels ScreenSpace
                }
        controlPoints =
            List.map
                (\p ->
                    { center = Point3d.Projection.toScreenSpace camera screenRectangle p.point
                    , point = p.point
                    , xSegment =
                        LineSegment2d.from
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.point)
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.xEndpoint)
                    , ySegment =
                        LineSegment2d.from
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.point)
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.yEndpoint)
                    , zSegment =
                        LineSegment2d.from
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.point)
                            (Point3d.Projection.toScreenSpace camera screenRectangle p.zEndpoint)
                    }
                )
                [ let
                    point =
                        spline
                            |> CubicSpline3d.fromNondegenerate
                            |> CubicSpline3d.firstControlPoint
                  in
                  { point = point
                  , xEndpoint = Point3d.translateIn Direction3d.positiveX (Length.meters 1) point
                  , yEndpoint = Point3d.translateIn Direction3d.positiveY (Length.meters 1) point
                  , zEndpoint = Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point
                  }
                , let
                    point =
                        spline
                            |> CubicSpline3d.fromNondegenerate
                            |> CubicSpline3d.secondControlPoint
                  in
                  { point = point
                  , xEndpoint = Point3d.translateIn Direction3d.positiveX (Length.meters 1) point
                  , yEndpoint = Point3d.translateIn Direction3d.positiveY (Length.meters 1) point
                  , zEndpoint = Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point
                  }
                , let
                    point =
                        spline
                            |> CubicSpline3d.fromNondegenerate
                            |> CubicSpline3d.thirdControlPoint
                  in
                  { point = point
                  , xEndpoint = Point3d.translateIn Direction3d.positiveX (Length.meters 1) point
                  , yEndpoint = Point3d.translateIn Direction3d.positiveY (Length.meters 1) point
                  , zEndpoint = Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point
                  }
                , let
                    point =
                        spline
                            |> CubicSpline3d.fromNondegenerate
                            |> CubicSpline3d.fourthControlPoint
                  in
                  { point = point
                  , xEndpoint = Point3d.translateIn Direction3d.positiveX (Length.meters 1) point
                  , yEndpoint = Point3d.translateIn Direction3d.positiveY (Length.meters 1) point
                  , zEndpoint = Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point
                  }
                ]

        viewControlPointDirSegment : String -> Int -> Plane3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> LineSegment2d Pixels ScreenSpace -> Svg Msg
        viewControlPointDirSegment color index plane dir segment =
            Geometry.Svg.lineSegment2d
                ([ Svg.Attributes.stroke color
                 , Svg.Attributes.strokeWidth "4"
                 , Svg.Attributes.class "track-editor-control-point-dir"

                 -- TODO
                 -- ([ Svg.Attributes.pointerEvents "all"
                 , Svg.Events.on "pointerdown" (decodePointerDown index plane dir)
                 , Svg.Events.on "pointerup" (decodePointerUp index)

                 -- , Svg.Events.on "keydown" (decodeKeyDown index)
                 -- ]
                 -- )
                 ]
                    ++ (case movingControlPoint of
                            Just details ->
                                if details.index == index then
                                    [ Svg.Attributes.style "cursor: grab"
                                    , Html.Attributes.property "___capturePointer" details.pointerId
                                    , Svg.Events.on "pointermove" (decodePointerMove index)
                                    ]

                                else
                                    []

                            Nothing ->
                                []
                       )
                )
                segment

        controlPointSvgs : Svg Msg
        controlPointSvgs =
            List.indexedMap
                (\index controlPoint ->
                    Svg.g
                        [ Svg.Attributes.class "track-editor-control-point"
                        ]
                        [ viewControlPointDirSegment
                            "red"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveZ)
                            Direction3d.positiveX
                            controlPoint.xSegment
                        , viewControlPointDirSegment
                            "green"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveZ)
                            Direction3d.positiveY
                            controlPoint.ySegment
                        , viewControlPointDirSegment
                            "blue"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveX)
                            Direction3d.positiveZ
                            controlPoint.zSegment
                        , Geometry.Svg.circle2d
                            [ Svg.Attributes.stroke "rgb(200, 255, 200)"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "rgba(0, 0, 0, 0)"
                            , Html.Attributes.attribute "tabindex" "0"
                            ]
                            (Circle2d.withRadius (Pixels.float 6) controlPoint.center)
                        ]
                )
                controlPoints
                |> Svg.g []
                |> Geometry.Svg.relativeTo topLeftFrame

        segmentSvgs : Svg.Svg msg
        segmentSvgs =
            List.foldl
                (\controlPoint acc ->
                    case acc of
                        Nothing ->
                            Just ( controlPoint.center, [] )

                        Just ( previousControlPoint, segments ) ->
                            Just
                                ( controlPoint.center
                                , Geometry.Svg.lineSegment2d
                                    [ Svg.Attributes.stroke "red"
                                    , Svg.Attributes.strokeWidth "0.5"
                                    , Svg.Attributes.strokeDasharray "5 5"
                                    , Svg.Attributes.class "track-editor-label-ignore"
                                    ]
                                    (LineSegment2d.from previousControlPoint controlPoint.center)
                                    :: segments
                                )
                )
                Nothing
                controlPoints
                |> Maybe.map Tuple.second
                |> Maybe.withDefault []
                |> Svg.g []
                |> Geometry.Svg.relativeTo topLeftFrame

        -- Used for converting from coordinates relative to the bottom-left
        -- corner of the 2D drawing into coordinates relative to the top-left
        -- corner (which is what SVG natively works in)
        topLeftFrame : Frame2d Pixels coordinates defines2
        topLeftFrame =
            Frame2d.reverseY (Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float viewSize.height)))
    in
    -- Create an SVG element with the projected points, lines and associated labels
    Svg.svg
        [ Html.Attributes.width (floor viewSize.width)
        , Html.Attributes.height (floor viewSize.height)
        , Svg.Attributes.class "track-editor-svg"
        ]
        [ controlPointSvgs
        , segmentSvgs
        ]


decodePointerDown : Int -> Plane3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Json.Decode.Decoder Msg
decodePointerDown index plane dir =
    Json.Decode.map3
        (\pointerId x y ->
            PointerDown
                { index = index
                , direction = dir
                , pointerId = pointerId
                , point = Point2d.pixels x y
                , plane = plane
                }
        )
        (Json.Decode.field "pointerId" Json.Decode.value)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


decodePointerUp : Int -> Json.Decode.Decoder Msg
decodePointerUp index =
    Json.Decode.succeed (PointerUp index)


decodePointerMove : Int -> Json.Decode.Decoder Msg
decodePointerMove index =
    Json.Decode.map2
        (\x y ->
            PointerMove
                index
                (Point2d.pixels x y)
        )
        (Json.Decode.at [ "clientX" ] Json.Decode.float)
        (Json.Decode.at [ "clientY" ] Json.Decode.float)


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
