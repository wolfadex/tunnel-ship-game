module Track exposing
    ( ActiveControlPoint
    , LengthError(..)
    , PotentialError(..)
    , Segment
    , Track
    , decode
    , encode
    , init
    , length
    , sample
    , view
    )

import Axis3d exposing (Axis3d)
import Color
import Coordinates
import CubicSpline3d
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
import Vector3d


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
    = ZeroSegments


init : Shape Coordinates.Flat -> ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) ) -> Track
init shape segments =
    Track
        { shape = shape
        , segments = segments
        , geometry = createTrackGeometry shape segments
        }


encode : Track -> Json.Encode.Value
encode (Track track) =
    Json.Encode.object
        [ ( "shape", Shape.encode track.shape )
        , ( "segments", encodeSegments track.segments )
        ]


encodeSegments : ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) ) -> Json.Encode.Value
encodeSegments ( first, rest ) =
    Json.Encode.list encodeSegment (first :: rest)


encodeSegment : Segment Meters Coordinates.World -> Json.Encode.Value
encodeSegment segment =
    Debug.todo "encodeSegment"


decode : Json.Decode.Decoder Track
decode =
    Json.Decode.map2
        (\shape segments ->
            Track
                { shape = shape
                , segments = segments
                , geometry = createTrackGeometry shape segments
                }
        )
        (Json.Decode.field "shape" Shape.decode)
        (Json.Decode.field "segments" decodeSegments)


decodeSegments : Json.Decode.Decoder ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) )
decodeSegments =
    Json.Decode.list decodeSegment
        |> Json.Decode.andThen
            (\segments ->
                case segments of
                    [] ->
                        Json.Decode.fail "Zero Segments"

                    first :: rest ->
                        Json.Decode.succeed ( first, rest )
            )


decodeSegment : Json.Decode.Decoder (Segment Meters Coordinates.World)
decodeSegment =
    Debug.todo ""


createTrackGeometry :
    Shape Coordinates.Flat
    ->
        ( Segment Meters Coordinates.World
        , List (Segment Meters Coordinates.World)
        )
    -> Scene3d.Entity Coordinates.World
createTrackGeometry initialShape segments =
    let
        segmentsInt : Int
        segmentsInt =
            segments
                |> lengthInteral
                |> Length.inMeters
                |> round
    in
    List.range 0 segmentsInt
        |> List.map
            (\i ->
                viewTunnelRing
                    initialShape
                    segments
                    (toFloat i |> Length.meters)
            )
        |> Scene3d.group


viewTunnelRing :
    Shape Coordinates.Flat
    ->
        ( Segment Meters Coordinates.World
        , List (Segment Meters Coordinates.World)
        )
    -> Length
    -> Scene3d.Entity Coordinates.World
viewTunnelRing shape segments dist =
    let
        sketchPlan =
            sampleAlong segments dist
                |> Frame3d.xzSketchPlane
    in
    shape
        |> Polygon2d.edges
        |> List.map
            (\segs ->
                LineSegment3d.on sketchPlan segs
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh (Scene3d.Material.color Color.lightPurple)


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


lengthInteral : ( Segment Meters Coordinates.World, List (Segment Meters Coordinates.World) ) -> Length
lengthInteral ( first, rest ) =
    List.foldl
        (\( leftSegment, _ ) len ->
            leftSegment
                |> CubicSpline3d.arcLengthParameterized { maxError = Length.meters 0.01 }
                |> CubicSpline3d.arcLength
                |> Quantity.plus len
        )
        (Length.meters 0)
        (first :: rest)


length : Track -> Length
length (Track track) =
    lengthInteral track.segments



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
