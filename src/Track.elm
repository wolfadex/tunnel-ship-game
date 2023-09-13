module Track exposing
    ( ActiveControlPoint
    , Track
    , decode
    , encode
    , init
    , length
    , moveControlPoint
    , newDebugFlags
    , sample
    , sketchPlaneAt
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
import Interval
import Json.Decode
import Json.Encode
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
import Util.Point3d
import Vector3d exposing (Vector3d)
import Visible exposing (Visible)


type Track
    = Track Internal


type alias Internal =
    { shape : Shape Coordinates.World
    , controlPoints : List (Point3d Meters Coordinates.World)
    , knots : List Float
    , geometry : Scene3d.Entity Coordinates.World
    }


encode : Track -> Json.Encode.Value
encode (Track track) =
    Json.Encode.object
        [ ( "shape", Shape.encode track.shape )
        , ( "controlPoints", encodeControlPoints track.controlPoints )
        , ( "knots", Json.Encode.list Json.Encode.float track.knots )
        ]


encodeControlPoints : List (Point3d Meters Coordinates.World) -> Json.Encode.Value
encodeControlPoints =
    Json.Encode.list (Util.Point3d.encode Length.inMeters)


decode : Json.Decode.Decoder Track
decode =
    Json.Decode.map3
        (\shape controlPoints knots ->
            Track
                { shape = shape
                , controlPoints = controlPoints
                , knots = knots
                , geometry = createTrackGeometry knots Nothing shape controlPoints
                }
        )
        (Json.Decode.field "shape" Shape.decode)
        (Json.Decode.field "controlPoints" decodeControlPoints)
        (Json.Decode.field "knots" (Json.Decode.list Json.Decode.float))


decodeControlPoints : Json.Decode.Decoder (List (Point3d Meters Coordinates.World))
decodeControlPoints =
    Json.Decode.list (Util.Point3d.decode Length.meters)


init : Shape Coordinates.World -> Maybe DebugFlags -> Track
init shape debugFlags =
    let
        knots =
            [ 0, 0, 2, 3, 4, 5, 6, 8, 12, 12 ]

        controlPoints =
            [ Point3d.meters 1 8 0
            , Point3d.meters 4 5 0
            , Point3d.meters 2 4 0
            , Point3d.meters 4 1 0
            , Point3d.meters 8 2 0
            , Point3d.meters 5 6 0
            , Point3d.meters 8 9 0
            , Point3d.meters 9 7 0
            , Point3d.meters 9 4 0
            ]
    in
    Track
        { shape = shape
        , controlPoints = controlPoints
        , knots = knots
        , geometry = createTrackGeometry knots debugFlags shape controlPoints
        }


carlPath : CubicSpline3d Meters coordinates -> CubicSpline3d.Nondegenerate Meters coordinates
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


createTrackGeometry : List Float -> Maybe DebugFlags -> Shape coordinates -> List (Point3d Meters coordinates) -> Scene3d.Entity coordinates
createTrackGeometry knots debugFlags initialShape controlPoints =
    let
        arcLength : Float
        arcLength =
            controlPoints
                |> CubicSpline3d.bSplineSegments knots
                |> List.map
                    (carlPath
                        >> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                        >> CubicSpline3d.arcLength
                        >> Length.inKilometers
                    )
                |> List.sum

        segmentsFloat : Float
        segmentsFloat =
            arcLength * 1500

        segmentsInt : Int
        segmentsInt =
            round segmentsFloat
    in
    [ viewIfVisible
        (debugFlags
            |> Maybe.map .trackPathVisible
            |> Maybe.withDefault Visible.Visible
        )
        [ createTrackPathGeometry knots controlPoints segmentsInt segmentsFloat ]
    , viewIfVisible
        (debugFlags
            |> Maybe.map .trackPathDownDirectionVisible
            |> Maybe.withDefault Visible.Visible
        )
        [ createTrackDownGeometry knots controlPoints segmentsInt segmentsFloat ]
    , List.range 0 segmentsInt
        |> List.map (\i -> viewTunnelRing knots initialShape controlPoints (toFloat i / segmentsFloat))
        |> viewIfVisible
            (debugFlags
                |> Maybe.map .tunnelVisible
                |> Maybe.withDefault Visible.Visible
            )
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


createTrackPathGeometry : List Float -> List (Point3d Meters coordinates) -> Int -> Float -> Scene3d.Entity coordinates
createTrackPathGeometry knots controlPoints segmentsInt segmentsFloat =
    List.map2
        (\knot segment ->
            List.range 0 5
                |> List.map
                    (\i ->
                        let
                            dist =
                                toFloat i / 5

                            ( point, dir ) =
                                CubicSpline3d.sample segment dist

                            vec : Vector3d Meters coordinates
                            vec =
                                dir
                                    |> Vector3d.withLength (Length.meters 1)
                                    |> Vector3d.scaleBy (1 / Interval.width knot)
                                    |> Vector3d.scaleBy 0.5
                        in
                        LineSegment3d.from point
                            (point
                                |> Point3d.translateBy vec
                            )
                    )
        )
        (CubicSpline3d.bSplineIntervals knots)
        (CubicSpline3d.bSplineSegments knots controlPoints
            |> List.map carlPath
        )
        |> List.concat
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.red)


createTrackDownGeometry : List Float -> List (Point3d Meters coordinates) -> Int -> Float -> Scene3d.Entity coordinates
createTrackDownGeometry knots controlPoints segmentsInt segmentsFloat =
    sketchPlaneAtInternal knots controlPoints 555
        |> List.map
            (\plane ->
                let
                    dir =
                        plane
                            |> SketchPlane3d.yDirection

                    p =
                        plane
                            |> SketchPlane3d.originPoint
                in
                LineSegment3d.from p
                    (p
                        |> Point3d.translateIn dir (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.green)


viewTunnelRing : List Float -> Shape coordinates -> List (Point3d Meters coordinates) -> Float -> Scene3d.Entity coordinates
viewTunnelRing knots shape controlPoints dist =
    shape
        |> Polygon2d.edges
        |> List.concatMap
            (\ls ->
                sketchPlaneAtInternal knots controlPoints dist
                    |> List.map (\plane -> LineSegment3d.on plane ls)
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.lightPurple)


sketchPlaneAtInternal : List Float -> List (Point3d Meters coordinates) -> Float -> List (SketchPlane3d Meters coordinates defines)
sketchPlaneAtInternal knots controlPoints _ =
    List.map2
        (\knot segment ->
            List.range 0 5
                |> List.map
                    (\i ->
                        let
                            dist =
                                toFloat i / 5

                            ( point, dir ) =
                                CubicSpline3d.sample segment dist
                        in
                        SketchPlane3d.through point dir
                    )
        )
        (CubicSpline3d.bSplineIntervals knots)
        (CubicSpline3d.bSplineSegments knots controlPoints
            |> List.map carlPath
        )
        |> List.concat


sketchPlaneAt : Track -> Length -> SketchPlane3d Meters Coordinates.World defines
sketchPlaneAt (Track track) dist =
    -- let
    --     arcLength =
    --         CubicSpline3d.arcLengthParameterized
    --             { maxError = Length.meters 0.01 }
    --             track.path
    --     ( center, normal ) =
    --         CubicSpline3d.sampleAlong arcLength dist
    -- in
    -- SketchPlane3d.through center normal
    Debug.todo ""



-- SketchPlane3d.unsafe
--     { originPoint = center
--     , xDirection =
--         Vector3d.cross
--             (Direction3d.positiveZ
--                 |> Direction3d.toVector
--             )
--             (normal
--                 |> Direction3d.toVector
--             )
--             |> Vector3d.direction
--             |> Maybe.withDefault Direction3d.positiveX
--     , yDirection = Direction3d.positiveZ
--     }


newDebugFlags : DebugFlags -> Track -> Track
newDebugFlags debugFlags (Track track) =
    Track
        { track
            | geometry = createTrackGeometry track.knots (Just debugFlags) track.shape track.controlPoints
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
        newControlPoints : List (Point3d Meters Coordinates.World)
        newControlPoints =
            List.indexedMap
                (\i oldControlPoint ->
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
                )
                track.controlPoints
    in
    Track
        { track
            | controlPoints = newControlPoints
            , geometry = createTrackGeometry track.knots (Just debugFlags) track.shape newControlPoints
        }


view : Track -> Scene3d.Entity Coordinates.World
view (Track track) =
    track.geometry


length : Track -> Length
length (Track track) =
    track.controlPoints
        |> CubicSpline3d.bSplineSegments track.knots
        |> List.foldl
            (carlPath
                >> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                >> CubicSpline3d.arcLength
                >> Quantity.plus
            )
            (Length.meters 0)



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
            track.controlPoints
                |> List.map
                    (\point ->
                        { center = Point3d.Projection.toScreenSpace camera screenRectangle point
                        , point = point
                        , xSegment =
                            LineSegment2d.from
                                (Point3d.Projection.toScreenSpace camera screenRectangle point)
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (Point3d.translateIn Direction3d.positiveX (Length.meters 1) point)
                                )
                        , ySegment =
                            LineSegment2d.from
                                (Point3d.Projection.toScreenSpace camera screenRectangle point)
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (Point3d.translateIn Direction3d.positiveY (Length.meters 1) point)
                                )
                        , zSegment =
                            LineSegment2d.from
                                (Point3d.Projection.toScreenSpace camera screenRectangle point)
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (Point3d.translateIn Direction3d.positiveZ (Length.meters 1) point)
                                )
                        }
                    )

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

        -- , segmentSvgs
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
    -- let
    --     arcLength =
    --         CubicSpline3d.arcLengthParameterized
    --             { maxError = Length.meters 0.01 }
    --             track.path
    -- in
    -- CubicSpline3d.sampleAlong arcLength dist
    Debug.todo ""
