module Track exposing
    ( ActiveControlPoint
    , LengthError(..)
    , Potential
    , PotentialError(..)
    , Track
    , decode
    , encode
    , fromPotential
    , init
    , length
    , moveControlPoint
    , newDebugFlags
    , potentialLength
    , rotateControlPoint
    , sample
    , samplePotential
    , view
    , viewControlPoints
    , viewPotential
    )

import Angle
import Arc3d exposing (Arc3d)
import Axis3d exposing (Axis3d)
import Block3d
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Coordinates
import CubicSpline3d exposing (CubicSpline3d)
import DebugFlags exposing (DebugFlags)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
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
import Util.Debug
import Util.Frame3d
import Util.Point3d
import Util.Result
import Vector3d exposing (Vector3d)
import Visible exposing (Visible)


type Potential
    = Potential Internal


type alias DefinesLocal =
    { defines : Coordinates.World }


type Local
    = Local Never


type alias Internal =
    { shape : Shape Coordinates.Flat
    , controlPoints : List (Frame3d Meters Coordinates.World DefinesLocal)
    , knots : List Float
    , geometry : Scene3d.Entity Coordinates.World
    }


type Track
    = Track InternalTrack


type alias InternalTrack =
    { shape : Shape Coordinates.Flat
    , segments :
        ( Segment Meters Coordinates.World
        , List (Segment Meters Coordinates.World)
        )
    , geometry : Scene3d.Entity Coordinates.World
    }


type PotentialError
    = PENotNondegenerate (Point3d Meters Coordinates.World)
    | ZeroSegments


fromPotential : Potential -> Result PotentialError Track
fromPotential (Potential potential) =
    let
        segmentsRes : Result (Point3d Meters Coordinates.World) (List (Segment Meters Coordinates.World))
        segmentsRes =
            potential.controlPoints
                |> List.map
                    (\frame ->
                        ( frame
                            |> Frame3d.originPoint
                        , frame
                            |> Frame3d.originPoint
                            |> Point3d.translateIn (Frame3d.xDirection frame) (Length.millimeters 1)
                        )
                    )
                |> List.unzip
                |> (\( a, b ) ->
                        case carlFn a of
                            Ok left ->
                                case carlFn b of
                                    Ok right ->
                                        Ok (List.map2 Tuple.pair left right)

                                    Err err ->
                                        Err err

                            Err err ->
                                Err err
                   )

        carlFn =
            CubicSpline3d.bSplineSegments potential.knots
                >> List.map CubicSpline3d.nondegenerate
                >> Util.Result.combine
    in
    segmentsRes
        |> Result.mapError PENotNondegenerate
        |> Result.andThen
            (\segments ->
                case segments of
                    [] ->
                        Err ZeroSegments

                    first :: rest ->
                        { shape = potential.shape
                        , segments = ( first, rest )
                        , geometry =
                            createTrackGeometry
                                potential.knots
                                Nothing
                                potential.shape
                                potential.controlPoints
                        }
                            |> Track
                            |> Ok
            )


encode : Potential -> Json.Encode.Value
encode (Potential track) =
    Json.Encode.object
        [ ( "shape", Shape.encode track.shape )
        , ( "controlPoints", encodeControlPoints track.controlPoints )
        , ( "knots", Json.Encode.list Json.Encode.float track.knots )
        ]


encodeControlPoints : List (Frame3d Meters Coordinates.World DefinesLocal) -> Json.Encode.Value
encodeControlPoints =
    Json.Encode.list (Util.Frame3d.encode Length.inMeters)


decode : Json.Decode.Decoder Potential
decode =
    Json.Decode.map3
        (\shape controlPoints knots ->
            Potential
                { shape = shape
                , controlPoints = controlPoints
                , knots = knots
                , geometry = createTrackGeometry knots Nothing shape controlPoints
                }
        )
        (Json.Decode.field "shape" Shape.decode)
        (Json.Decode.field "controlPoints" decodeControlPoints)
        (Json.Decode.field "knots" (Json.Decode.list Json.Decode.float))


decodeControlPoints : Json.Decode.Decoder (List (Frame3d Meters Coordinates.World DefinesLocal))
decodeControlPoints =
    Json.Decode.list (Util.Frame3d.decode Length.meters)


init : Shape Coordinates.Flat -> Maybe DebugFlags -> Potential
init shape debugFlags =
    let
        knots =
            [ 0, 0, 1, 2, 3, 4, 5, 6, 7, 7 ]

        controlPoints : List (Frame3d Meters Coordinates.World DefinesLocal)
        controlPoints =
            [ Frame3d.unsafe
                { originPoint = Point3d.meters 0 0 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 2 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 4 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 6 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 8 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 10 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 12 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 14 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            , Frame3d.unsafe
                { originPoint = Point3d.meters 0 16 0
                , xDirection = Direction3d.positiveX
                , yDirection = Direction3d.positiveY
                , zDirection = Direction3d.positiveZ
                }
            ]
    in
    Potential
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


createTrackGeometry : List Float -> Maybe DebugFlags -> Shape Coordinates.Flat -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Scene3d.Entity Coordinates.World
createTrackGeometry knots debugFlags initialShape controlPoints =
    let
        segmentsInt : Int
        segmentsInt =
            potentialLengthInternal { controlPoints = controlPoints, knots = knots }
                |> Result.map (Length.inMeters >> round)
                |> Result.withDefault 0

        segmentsFloat : Float
        segmentsFloat =
            toFloat segmentsInt
    in
    [ viewIfVisible
        (debugFlags
            |> Maybe.map .trackPathDownDirectionVisible
            |> Maybe.withDefault Visible.Visible
        )
        [ createTrackUpGeometry knots controlPoints segmentsInt segmentsFloat ]

    -- , viewIfVisible
    --     (debugFlags
    --         |> Maybe.map .trackPathVisible
    --         |> Maybe.withDefault Visible.Visible
    --     )
    --     [ createTrackPathGeometry knots controlPoints segmentsInt segmentsFloat ]
    -- , viewIfVisible
    --     (debugFlags
    --         |> Maybe.map .trackPathDownDirectionVisible
    --         |> Maybe.withDefault Visible.Visible
    --     )
    --     [ createTrackRightGeometry knots controlPoints segmentsInt segmentsFloat ]
    , List.range 0 segmentsInt
        |> List.map
            (\i ->
                viewTunnelRing
                    knots
                    initialShape
                    controlPoints
                    (toFloat i |> Length.meters)
            )
        |> viewIfVisible
            (debugFlags
                |> Maybe.map .tunnelVisible
                |> Maybe.withDefault Visible.Visible
            )

    -- , List.range 0 segmentsInt
    --     |> List.map
    --         (\i ->
    --             viewBox
    --                 knots
    --                 initialShape
    --                 controlPoints
    --                 (toFloat i |> Length.meters)
    --         )
    --     |> viewIfVisible
    --         (debugFlags
    --             |> Maybe.map .tunnelVisible
    --             |> Maybe.withDefault Visible.Visible
    --         )
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


createTrackPathGeometry : List Float -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Int -> Float -> Scene3d.Entity Coordinates.World
createTrackPathGeometry knots controlPoints segmentsInt _ =
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                let
                    dist =
                        i
                            |> toFloat
                            |> Length.meters

                    frame =
                        samplePotentialAtInternal knots controlPoints dist

                    point =
                        Frame3d.originPoint frame
                in
                LineSegment3d.from point
                    (point
                        |> Point3d.translateIn (Frame3d.yDirection frame) (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.green)


createTrackUpGeometry : List Float -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Int -> Float -> Scene3d.Entity Coordinates.World
createTrackUpGeometry knots controlPoints segmentsInt _ =
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                let
                    dist =
                        i
                            |> toFloat
                            |> Length.meters

                    frame =
                        samplePotentialAtInternal knots controlPoints dist

                    point =
                        Frame3d.originPoint frame
                in
                LineSegment3d.from point
                    (point
                        |> Point3d.translateIn (Frame3d.zDirection frame) (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.blue)


createTrackRightGeometry : List Float -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Int -> Float -> Scene3d.Entity Coordinates.World
createTrackRightGeometry knots controlPoints segmentsInt _ =
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                let
                    dist =
                        i
                            |> toFloat
                            |> Length.meters

                    frame =
                        samplePotentialAtInternal knots controlPoints dist

                    point =
                        Frame3d.originPoint frame
                in
                LineSegment3d.from point
                    (point
                        |> Point3d.translateIn (Frame3d.xDirection frame) (Length.meters 1)
                    )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.red)


viewTunnelRing : List Float -> Shape Coordinates.Flat -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Length -> Scene3d.Entity Coordinates.World
viewTunnelRing knots shape controlPoints dist =
    let
        sketchPlan =
            samplePotentialAtInternal
                knots
                controlPoints
                dist
                |> Frame3d.xzSketchPlane
    in
    shape
        |> Polygon2d.edges
        |> List.map
            (\segments ->
                LineSegment3d.on sketchPlan segments
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.lightPurple)


viewBox : List Float -> Shape Coordinates.Flat -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Length -> Scene3d.Entity Coordinates.World
viewBox knots shape controlPoints dist =
    let
        frame =
            samplePotentialAtInternal
                knots
                controlPoints
                dist
    in
    Block3d.centeredOn
        frame
        ( Length.meters 0.5
        , Length.meters 0.5
        , Length.meters 0.5
        )
        |> Scene3d.block (Scene3d.Material.color Color.lightPurple)


samplePotentialAtInternal : List Float -> List (Frame3d Meters Coordinates.World DefinesLocal) -> Length -> Frame3d Meters Coordinates.World DefinesLocal
samplePotentialAtInternal knots controlPoints dist =
    let
        segmentsRes : Result (Point3d Meters Coordinates.World) (List (Segment Meters Coordinates.World))
        segmentsRes =
            controlPoints
                |> List.map
                    (\frame ->
                        ( frame
                            |> Frame3d.originPoint
                        , frame
                            |> Frame3d.originPoint
                            |> Point3d.translateIn (Frame3d.xDirection frame) Length.millimeter
                        )
                    )
                |> List.unzip
                |> (\( a, b ) ->
                        case carlFn a of
                            Ok left ->
                                case carlFn b of
                                    Ok right ->
                                        Ok (List.map2 Tuple.pair left right)

                                    Err err ->
                                        Err err

                            Err err ->
                                Err err
                   )

        carlFn =
            CubicSpline3d.bSplineSegments knots
                >> List.map CubicSpline3d.nondegenerate
                >> Util.Result.combine
    in
    case segmentsRes of
        Err err ->
            Debug.todo "samplePotentialAtInternal"

        Ok [] ->
            Debug.todo "samplePotentialAtInternal"

        Ok (first :: rest) ->
            sampleAlong ( first, rest ) dist


type alias Segment units coordinates =
    ( CubicSpline3d.Nondegenerate units coordinates
    , CubicSpline3d.Nondegenerate units coordinates
    )


sampleAlong : ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) ) -> Length -> Frame3d Meters Coordinates.World DefinesLocal
sampleAlong ( ( left, right ), rest ) dist =
    let
        arcLengthParamLeft =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                left

        arcLengthParamRight =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                right

        arcLengthLeft =
            CubicSpline3d.arcLength arcLengthParamLeft
    in
    -- TODO: All of the below will break if dist is negative
    if dist |> Quantity.lessThanOrEqualTo arcLengthLeft then
        let
            ( leftPoint, leftTangent ) =
                CubicSpline3d.sampleAlong arcLengthParamLeft dist

            ( rightPoint, rightTangent ) =
                -- CubicSpline3d.sample right
                --     (Length.inKilometers dist / Length.inKilometers arcLengthLeft)
                CubicSpline3d.sampleAlong arcLengthParamRight dist

            yDirection =
                Direction3d.toVector leftTangent
                    |> Vector3d.plus (Direction3d.toVector rightTangent)
                    |> Vector3d.direction
                    |> Maybe.withDefault leftTangent

            xDirection =
                Direction3d.from leftPoint rightPoint
                    |> Maybe.withDefault Direction3d.positiveX
        in
        Frame3d.unsafe
            { originPoint = leftPoint
            , yDirection = yDirection
            , xDirection = xDirection
            , zDirection =
                Direction3d.toVector xDirection
                    |> Vector3d.cross
                        (Direction3d.toVector yDirection)
                    |> Vector3d.direction
                    |> Maybe.withDefault Direction3d.positiveZ
            }

    else
        case rest of
            [] ->
                sampleAlong ( ( left, right ), rest ) (dist |> Quantity.minus arcLengthLeft)

            first :: last ->
                sampleAlong ( first, last ) (dist |> Quantity.minus arcLengthLeft)


newDebugFlags : DebugFlags -> Potential -> Potential
newDebugFlags debugFlags (Potential track) =
    Potential
        { track
            | geometry = createTrackGeometry track.knots (Just debugFlags) track.shape track.controlPoints
        }


moveControlPoint :
    { debugFlags : DebugFlags
    , movingControlPoint : ActiveControlPoint
    , camera : Camera3d Meters Coordinates.World
    , screenRectangle : Rectangle2d Pixels Coordinates.Screen
    }
    -> Potential
    -> Potential
moveControlPoint { debugFlags, movingControlPoint, camera, screenRectangle } (Potential track) =
    let
        newControlPoints : List (Frame3d Meters Coordinates.World DefinesLocal)
        newControlPoints =
            List.indexedMap
                (\i oldControlPoint ->
                    if movingControlPoint.index == i then
                        let
                            axis =
                                Axis3d.through (Frame3d.originPoint oldControlPoint) movingControlPoint.direction
                        in
                        movingControlPoint.point
                            |> Camera3d.ray camera screenRectangle
                            |> Axis3d.intersectionWithPlane movingControlPoint.plane
                            |> Maybe.andThen
                                (\p2 ->
                                    let
                                        oldCenter =
                                            Frame3d.originPoint oldControlPoint
                                    in
                                    Point3d.projectOntoAxis axis p2
                                        |> Direction3d.from oldCenter
                                        |> Maybe.map
                                            (\dir ->
                                                Frame3d.translateIn
                                                    dir
                                                    (Point3d.distanceFrom oldCenter p2)
                                                    oldControlPoint
                                            )
                                )
                            |> Maybe.withDefault oldControlPoint

                    else
                        oldControlPoint
                )
                track.controlPoints
    in
    Potential
        { track
            | controlPoints = newControlPoints
            , geometry = createTrackGeometry track.knots (Just debugFlags) track.shape newControlPoints
        }


rotateControlPoint :
    { debugFlags : DebugFlags
    , movingControlPoint : ActiveControlPoint
    , previousControlPoint : Point2d Pixels Coordinates.Screen
    , camera : Camera3d Meters Coordinates.World
    , screenRectangle : Rectangle2d Pixels Coordinates.Screen
    }
    -> Potential
    -> Potential
rotateControlPoint { debugFlags, movingControlPoint, previousControlPoint, camera, screenRectangle } (Potential track) =
    let
        newControlPoints : List (Frame3d Meters Coordinates.World DefinesLocal)
        newControlPoints =
            List.indexedMap
                (\i oldControlPoint ->
                    if movingControlPoint.index == i then
                        let
                            newCP =
                                movingControlPoint.point
                                    |> Camera3d.ray camera screenRectangle
                                    |> Axis3d.intersectionWithPlane movingControlPoint.plane

                            oldCP =
                                previousControlPoint
                                    |> Camera3d.ray camera screenRectangle
                                    |> Axis3d.intersectionWithPlane movingControlPoint.plane
                        in
                        Maybe.map2
                            (\newP oldP ->
                                let
                                    angle =
                                        Point3d.signedDistanceAlong (Axis3d.moveTo oldP movingControlPoint.rotationAxis) newP
                                            |> Length.inMeters
                                            |> (*) 2
                                            |> Debug.log "angle"
                                            |> Angle.degrees
                                in
                                Frame3d.rotateAround movingControlPoint.rotationAxis angle oldControlPoint
                            )
                            newCP
                            oldCP
                            |> Maybe.withDefault oldControlPoint

                    else
                        oldControlPoint
                )
                track.controlPoints
    in
    Potential
        { track
            | controlPoints = newControlPoints
            , geometry = createTrackGeometry track.knots (Just debugFlags) track.shape newControlPoints
        }


view : Track -> Scene3d.Entity Coordinates.World
view (Track track) =
    track.geometry


viewPotential : Potential -> Scene3d.Entity Coordinates.World
viewPotential (Potential track) =
    track.geometry


type LengthError
    = NotNondegenerate (Point3d Meters Coordinates.World)


potentialLength : Potential -> Result LengthError Length
potentialLength (Potential potential) =
    potentialLengthInternal potential


potentialLengthInternal : { a | controlPoints : List (Frame3d Meters Coordinates.World DefinesLocal), knots : List Float } -> Result LengthError Length
potentialLengthInternal { controlPoints, knots } =
    controlPoints
        |> List.map Frame3d.originPoint
        |> CubicSpline3d.bSplineSegments knots
        |> List.map CubicSpline3d.nondegenerate
        |> Util.Result.combine
        |> Result.mapError NotNondegenerate
        |> Result.map
            (List.foldl
                (CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                    >> CubicSpline3d.arcLength
                    >> Quantity.plus
                )
                (Length.meters 0)
            )


length : Track -> Length
length (Track track) =
    let
        ( first, rest ) =
            track.segments
    in
    List.foldl
        (\( leftSegment, _ ) len ->
            leftSegment
                |> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                |> CubicSpline3d.arcLength
                |> Quantity.plus len
        )
        (Length.meters 0)
        (first :: rest)



-- Editing


viewControlPoints :
    { viewSize : { width : Float, height : Float }
    , camera : Camera3d Meters Coordinates.World
    , movingControlPoint : Maybe ActiveControlPoint
    , onPointerDown : ActiveControlPoint -> msg
    , onPointerUp : Int -> msg
    , onPointerMove : Int -> Point2d Pixels Coordinates.Screen -> msg
    }
    -> Potential
    -> Html msg
viewControlPoints { viewSize, camera, movingControlPoint, onPointerDown, onPointerUp, onPointerMove } (Potential track) =
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
                        { center =
                            point
                                |> Frame3d.originPoint
                                |> Point3d.Projection.toScreenSpace camera screenRectangle
                        , point = Frame3d.originPoint point
                        , xSegment =
                            LineSegment2d.from
                                (point
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (point
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveX (Length.meters 1)
                                    )
                                )
                        , ySegment =
                            LineSegment2d.from
                                (point
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (point
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveY (Length.meters 1)
                                    )
                                )
                        , zSegment =
                            LineSegment2d.from
                                (point
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (point
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1)
                                    )
                                )
                        }
                    )

        viewControlPointDirSegment :
            String
            -> Int
            -> Plane3d Meters Coordinates.World
            -> Direction3d Coordinates.World
            -> Axis3d Meters Coordinates.World
            -> LineSegment2d Pixels Coordinates.Screen
            -> Svg msg
        viewControlPointDirSegment color index plane dir rotAxis segment =
            Geometry.Svg.lineSegment2d
                ([ Svg.Attributes.stroke color
                 , Svg.Attributes.strokeWidth "4"
                 , Svg.Attributes.class "track-editor-control-point-dir"

                 -- TODO
                 -- ([ Svg.Attributes.pointerEvents "all"
                 , Svg.Events.on "pointerdown" (decodePointerDown onPointerDown index plane dir rotAxis)
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
                            (Direction3d.positiveX |> Axis3d.through controlPoint.point)
                            controlPoint.xSegment
                        , viewControlPointDirSegment
                            "green"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveZ)
                            Direction3d.positiveY
                            (Direction3d.positiveY |> Axis3d.through controlPoint.point)
                            controlPoint.ySegment
                        , viewControlPointDirSegment
                            "blue"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveX)
                            Direction3d.positiveZ
                            (Direction3d.positiveZ |> Axis3d.through controlPoint.point)
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


decodePointerDown :
    (ActiveControlPoint -> msg)
    -> Int
    -> Plane3d Meters Coordinates.World
    -> Direction3d Coordinates.World
    -> Axis3d Meters Coordinates.World
    -> Json.Decode.Decoder msg
decodePointerDown handler index plane dir rotAxis =
    Json.Decode.map3
        (\pointerId x y ->
            handler
                { index = index
                , direction = dir
                , pointerId = pointerId
                , point = Point2d.pixels x y
                , plane = plane
                , rotationAxis = rotAxis
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
    , rotationAxis : Axis3d Meters Coordinates.World
    }


samplePotential : Potential -> Length -> Frame3d Meters Coordinates.World DefinesLocal
samplePotential (Potential track) dist =
    -- let
    --     arcLength =
    --         CubicSpline3d.arcLengthParameterized
    --             { maxError = Length.meters 0.01 }
    --             track.path
    -- in
    -- CubicSpline3d.sampleAlong arcLength dist
    Debug.todo ""


sample : Track -> Length -> Frame3d Meters Coordinates.World DefinesLocal
sample (Track track) dist =
    sampleAlong track.segments dist
