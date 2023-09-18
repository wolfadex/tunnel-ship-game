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
    , sample
    , view
    )

import Axis3d exposing (Axis3d)
import Color
import Coordinates
import CubicSpline3d
import DebugFlags exposing (DebugFlags)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Json.Decode
import Json.Encode
import Length exposing (Length, Meters)
import LineSegment3d
import Pixels exposing (Pixels)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d
import Quantity
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Shape exposing (Shape)
import Track.Potential
import Util.Frame3d
import Util.Result
import Vector3d
import Visible exposing (Visible)


type Potential
    = Potential Internal


type alias Internal =
    { shape : Shape Coordinates.Flat
    , controlPoints : List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal)
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


fromPotential : Track.Potential.Potential -> Result PotentialError Track
fromPotential (Track.Potential.Potential potential) =
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


encodeControlPoints : List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal) -> Json.Encode.Value
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


decodeControlPoints : Json.Decode.Decoder (List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal))
decodeControlPoints =
    Json.Decode.list (Util.Frame3d.decode Length.meters)


init : Shape Coordinates.Flat -> Maybe DebugFlags -> Potential
init shape debugFlags =
    let
        knots =
            [ 0, 0, 1, 2, 3, 4, 5, 6, 7, 7 ]

        controlPoints : List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal)
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


createTrackGeometry : List Float -> Maybe DebugFlags -> Shape Coordinates.Flat -> List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal) -> Scene3d.Entity Coordinates.World
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


createTrackUpGeometry : List Float -> List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal) -> Int -> Float -> Scene3d.Entity Coordinates.World
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


viewTunnelRing : List Float -> Shape Coordinates.Flat -> List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal) -> Length -> Scene3d.Entity Coordinates.World
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


samplePotentialAtInternal : List Float -> List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal) -> Length -> Frame3d Meters Coordinates.World Coordinates.DefinesLocal
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
        Err _ ->
            Debug.todo "samplePotentialAtInternal"

        Ok [] ->
            Debug.todo "samplePotentialAtInternal"

        Ok (first :: rest) ->
            sampleAlong ( first, rest ) dist


type alias Segment units coordinates =
    ( CubicSpline3d.Nondegenerate units coordinates
    , CubicSpline3d.Nondegenerate units coordinates
    )


sampleAlong : ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) ) -> Length -> Frame3d Meters Coordinates.World Coordinates.DefinesLocal
sampleAlong ( ( left, right ), rest ) dist =
    let
        arcLengthParamLeft =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                left

        arcLengthLeft =
            CubicSpline3d.arcLength arcLengthParamLeft
    in
    -- TODO: All of the below will break if dist is negative
    if dist |> Quantity.lessThanOrEqualTo arcLengthLeft then
        let
            arcLengthParamRight =
                CubicSpline3d.arcLengthParameterized
                    { maxError = Length.meters 0.01 }
                    right

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


view : Track -> Scene3d.Entity Coordinates.World
view (Track track) =
    track.geometry


type LengthError
    = NotNondegenerate (Point3d Meters Coordinates.World)


potentialLengthInternal : { a | controlPoints : List (Frame3d Meters Coordinates.World Coordinates.DefinesLocal), knots : List Float } -> Result LengthError Length
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


type alias ActiveControlPoint =
    { pointerId : Json.Decode.Value
    , index : Int
    , point : Point2d Pixels Coordinates.Screen
    , direction : Direction3d Coordinates.World
    , plane : Plane3d Meters Coordinates.World
    , rotationAxis : Axis3d Meters Coordinates.World
    }


sample : Track -> Length -> Frame3d Meters Coordinates.World Coordinates.DefinesLocal
sample (Track track) dist =
    sampleAlong track.segments dist
