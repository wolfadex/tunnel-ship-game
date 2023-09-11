module Track exposing
    ( ActiveControlPoint
    , Track
    , init
    , length
    , moveControlPoint
    , newDebugFlags
    , sample
    , view
    , viewControlPoints
    )

import Axis3d
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Coordinates
import CubicSpline3d exposing (CubicSpline3d)
import DebugFlags exposing (DebugFlags)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d
import Pixels exposing (Pixels)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Polygon2d
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Shape exposing (Shape)
import SketchPlane3d exposing (SketchPlane3d)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Visible exposing (Visible)


type Track
    = Track Internal


type alias Internal =
    { shape : Shape Coordinates.World
    , path : CubicSpline3d.Nondegenerate Meters Coordinates.World
    , geometry : Scene3d.Entity Coordinates.World
    }


init : Shape Coordinates.World -> DebugFlags -> Track
init shape debugFlags =
    let
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
    in
    Track
        { shape = shape
        , path = path
        , geometry = createTrackGeometry debugFlags shape path
        }


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
        |> List.concat
        |> Scene3d.group


viewIfVisible : Visible -> List (Scene3d.Entity coordinates) -> List (Scene3d.Entity coordinates)
viewIfVisible visible entities =
    case visible of
        Visible.Visible ->
            entities

        Visible.Hidden ->
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


viewTunnelRing : Shape coordinates -> CubicSpline3d.Nondegenerate Meters coordinates -> Float -> Scene3d.Entity coordinates
viewTunnelRing shape path dist =
    shape
        |> Polygon2d.edges
        |> List.map (LineSegment3d.on (sketchPlaneAt path dist))
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.lightPurple)


sketchPlaneAt : CubicSpline3d.Nondegenerate Meters coordinates -> Float -> SketchPlane3d Meters coordinates defines
sketchPlaneAt path dist =
    let
        ( center, normal ) =
            CubicSpline3d.sample path dist
    in
    SketchPlane3d.through center normal


newDebugFlags : DebugFlags -> Track -> Track
newDebugFlags debugFlags (Track track) =
    Track
        { track
            | geometry = createTrackGeometry debugFlags track.shape track.path
        }


moveControlPoint :
    { debugFlags : DebugFlags
    , movingControlPoint : ActiveControlPoint
    , camera : Camera3d Meters Coordinates.World
    , screenRectangle : Rectangle2d Pixels Coordinates.Screen
    }
    -> Track
    -> Track
moveControlPoint { debugFlags, movingControlPoint, camera, screenRectangle } (Track track) =
    let
        newControlPoint : Int -> Point3d Meters Coordinates.World
        newControlPoint i =
            let
                oldControlPoint =
                    track.path
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
            in
            if movingControlPoint.index == i then
                let
                    axis =
                        Axis3d.through oldControlPoint movingControlPoint.direction
                in
                movingControlPoint.point
                    |> Camera3d.ray camera screenRectangle
                    |> Axis3d.intersectionWithPlane movingControlPoint.plane
                    |> Maybe.map (Point3d.projectOntoAxis axis)
                    |> Maybe.withDefault oldControlPoint

            else
                oldControlPoint

        newPath =
            carlPath
                (CubicSpline3d.fromControlPoints
                    (newControlPoint 0)
                    (newControlPoint 1)
                    (newControlPoint 2)
                    (newControlPoint 3)
                )
    in
    Track
        { track
            | path = newPath
            , geometry = createTrackGeometry debugFlags track.shape newPath
        }


view : Track -> Scene3d.Entity Coordinates.World
view (Track track) =
    track.geometry


length : Track -> Length
length (Track track) =
    track.path
        |> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
        |> CubicSpline3d.arcLength



-- Editing


viewControlPoints :
    { viewSize : { width : Float, height : Float }
    , camera : Camera3d Meters Coordinates.World
    , movingControlPoint : Maybe ActiveControlPoint
    , onPointerDown : ActiveControlPoint -> msg
    , onPointerUp : Int -> msg
    , onPointerMove : Int -> Point2d Pixels Coordinates.Screen -> msg
    }
    -> Track
    -> Html msg
viewControlPoints { viewSize, camera, movingControlPoint, onPointerDown, onPointerUp, onPointerMove } (Track track) =
    let
        -- Take the 3D model for the logo and rotate it by the current angle
        -- rotatedLogo =
        --     blockEntity |> Scene3d.rotateAround Axis3d.z angle
        -- Defines the shape of the 'screen' that we will be using when
        --
        -- projecting 3D points into 2D
        screenRectangle : Rectangle2d Pixels Coordinates.Screen
        screenRectangle =
            Point2d.pixels viewSize.width viewSize.height
                |> Rectangle2d.from Point2d.origin

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        controlPoints :
            List
                { center : Point2d Pixels Coordinates.Screen
                , point : Point3d Meters Coordinates.World
                , xSegment : LineSegment2d Pixels Coordinates.Screen
                , ySegment : LineSegment2d Pixels Coordinates.Screen
                , zSegment : LineSegment2d Pixels Coordinates.Screen
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
                        track.path
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
                        track.path
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
                        track.path
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
                        track.path
                            |> CubicSpline3d.fromNondegenerate
                            |> CubicSpline3d.fourthControlPoint
                  in
                  { point = point
                  , xEndpoint = Point3d.translateIn Direction3d.positiveX (Length.meters 1) point
                  , yEndpoint = Point3d.translateIn Direction3d.positiveY (Length.meters 1) point
                  , zEndpoint = Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point
                  }
                ]

        viewControlPointDirSegment : String -> Int -> Plane3d Meters Coordinates.World -> Direction3d Coordinates.World -> LineSegment2d Pixels Coordinates.Screen -> Svg msg
        viewControlPointDirSegment color index plane dir segment =
            Geometry.Svg.lineSegment2d
                ([ Svg.Attributes.stroke color
                 , Svg.Attributes.strokeWidth "4"
                 , Svg.Attributes.class "track-editor-control-point-dir"

                 -- TODO
                 -- ([ Svg.Attributes.pointerEvents "all"
                 , Svg.Events.on "pointerdown" (decodePointerDown onPointerDown index plane dir)
                 , Svg.Events.on "pointerup" (decodePointerUp onPointerUp index)

                 -- , Svg.Events.on "keydown" (decodeKeyDown index)
                 -- ]
                 -- )
                 ]
                    ++ (case movingControlPoint of
                            Just details ->
                                if details.index == index then
                                    [ Svg.Attributes.style "cursor: grab"
                                    , Html.Attributes.property "___capturePointer" details.pointerId
                                    , Svg.Events.on "pointermove" (decodePointerMove onPointerMove index)
                                    ]

                                else
                                    []

                            Nothing ->
                                []
                       )
                )
                segment

        controlPointSvgs : Svg msg
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


decodePointerDown : (ActiveControlPoint -> msg) -> Int -> Plane3d Meters Coordinates.World -> Direction3d Coordinates.World -> Json.Decode.Decoder msg
decodePointerDown handler index plane dir =
    Json.Decode.map3
        (\pointerId x y ->
            handler
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


decodePointerUp : (Int -> msg) -> Int -> Json.Decode.Decoder msg
decodePointerUp handler index =
    Json.Decode.succeed (handler index)


decodePointerMove : (Int -> Point2d Pixels Coordinates.Screen -> msg) -> Int -> Json.Decode.Decoder msg
decodePointerMove handler index =
    Json.Decode.map2 (\x y -> handler index (Point2d.pixels x y))
        (Json.Decode.at [ "clientX" ] Json.Decode.float)
        (Json.Decode.at [ "clientY" ] Json.Decode.float)


type alias ActiveControlPoint =
    { pointerId : Json.Decode.Value
    , index : Int
    , point : Point2d Pixels Coordinates.Screen
    , direction : Direction3d Coordinates.World
    , plane : Plane3d Meters Coordinates.World
    }


sample : Track -> Length -> ( Point3d Meters Coordinates.World, Direction3d Coordinates.World )
sample (Track track) dist =
    let
        arcLength =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                track.path
    in
    CubicSpline3d.sampleAlong arcLength dist
