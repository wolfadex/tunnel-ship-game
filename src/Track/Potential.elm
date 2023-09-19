module Track.Potential exposing
    ( ActiveControlPoint
    , Control
    , EditType(..)
    , Effect
    , Internal
    , LengthError(..)
    , Msg
    , Potential(..)
    , Scope(..)
    , Segment
    , editControlPoint
    , editMode
    , init
    , length
    , newDebugFlags
    , setEditMode
    , toSegments
    , update
    , view
    , viewControlPoints
    )

import Angle
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Coordinates
import CubicSpline3d
import DebugFlags exposing (DebugFlags)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
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
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Update exposing (Update)
import Util.Result
import Vector3d
import Viewpoint3d
import Visible exposing (Visible)


type Potential
    = Potential Internal


type alias Internal =
    { shape : Shape Coordinates.Flat
    , controlPoints : List Control
    , knots : List Float
    , geometry : Scene3d.Entity Coordinates.World
    , editMode : ( EditType, Scope )
    }


type alias ControlFrame =
    Frame3d Meters Coordinates.World Coordinates.DefinesLocal


type Control
    = Selected ControlFrame
      -- | MovingGlobal
      --     { pointerId : Json.Decode.Value
      --     , index : Int
      --     , point : Point2d Pixels Coordinates.Screen
      --     , direction : Direction3d Coordinates.World
      --     , plane : Plane3d Meters Coordinates.World
      --     , editingFrame : ControlFrame
      --     , originalFrame : ControlFrame
      --     }
      -- | RotatingLocal
      --     { pointerId : Json.Decode.Value
      --     , index : Int
      --     , point : Point2d Pixels Coordinates.Screen
      --     , direction : Direction3d Coordinates.World
      --     , plane : Plane3d Meters Coordinates.World
      --     , rotationAxis : Axis3d Meters Coordinates.World
      --     , editingFrame : ControlFrame
      --     , originalFrame : ControlFrame
      --     }
    | Fixed ControlFrame


type alias ActiveControlPoint =
    { pointerId : Json.Decode.Value
    , index : Int
    , point : Point2d Pixels Coordinates.Screen
    , direction : Direction3d Coordinates.World
    , plane : Plane3d Meters Coordinates.World
    , rotationAxis : Axis3d Meters Coordinates.World
    }


type EditType
    = Move
    | Rotate


type Scope
    = Global
    | Local


init : Shape Coordinates.Flat -> Maybe DebugFlags -> Potential
init shape debugFlags =
    let
        knots =
            [ 0, 0, 1, 2, 3, 4, 5, 6, 7, 7 ]

        controlPoints : List Control
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
                |> List.map Fixed
    in
    Potential
        { shape = shape
        , controlPoints = controlPoints
        , knots = knots
        , geometry = createTrackGeometry knots debugFlags shape controlPoints
        , editMode = ( Move, Global )
        }


controlToFrame : Control -> ControlFrame
controlToFrame control =
    case control of
        -- MovingGlobal { originalFrame } ->
        --     originalFrame
        -- RotatingLocal { originalFrame } ->
        --     originalFrame
        Selected frame ->
            frame

        Fixed frame ->
            frame


createTrackGeometry : List Float -> Maybe DebugFlags -> Shape Coordinates.Flat -> List Control -> Scene3d.Entity Coordinates.World
createTrackGeometry knots debugFlags initialShape controlPoints =
    let
        segmentsInt : Int
        segmentsInt =
            lengthInternal { controlPoints = controlPoints, knots = knots }
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


type LengthError
    = NotNondegenerate (Point3d Meters Coordinates.World)


viewIfVisible : Visible -> List (Scene3d.Entity coordinates) -> List (Scene3d.Entity coordinates)
viewIfVisible visible entities =
    case visible of
        Visible.Visible ->
            entities

        Visible.Hidden ->
            []


createTrackUpGeometry : List Float -> List Control -> Int -> Float -> Scene3d.Entity Coordinates.World
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


createTrackRightGeometry : List Float -> List Control -> Int -> Float -> Scene3d.Entity Coordinates.World
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


viewTunnelRing : List Float -> Shape Coordinates.Flat -> List Control -> Length -> Scene3d.Entity Coordinates.World
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


samplePotentialAtInternal : List Float -> List Control -> Length -> ControlFrame
samplePotentialAtInternal knots controlPoints dist =
    let
        segmentsRes : Result (Point3d Meters Coordinates.World) (List (Segment Meters Coordinates.World))
        segmentsRes =
            controlPoints
                |> List.map
                    (\control ->
                        ( control
                            |> controlToFrame
                            |> Frame3d.originPoint
                        , control
                            |> controlToFrame
                            |> Frame3d.originPoint
                            |> Point3d.translateIn (Frame3d.xDirection (controlToFrame control)) Length.millimeter
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


toSegments : Potential -> Result (Point3d Meters Coordinates.World) (List (Segment Meters Coordinates.World))
toSegments (Potential potential) =
    let
        carlFn =
            CubicSpline3d.bSplineSegments potential.knots
                >> List.map CubicSpline3d.nondegenerate
                >> Util.Result.combine
    in
    potential.controlPoints
        |> List.map
            (\control ->
                ( control
                    |> controlToFrame
                    |> Frame3d.originPoint
                , control
                    |> controlToFrame
                    |> Frame3d.originPoint
                    |> Point3d.translateIn (Frame3d.xDirection (controlToFrame control)) Length.millimeter
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


editControlPoint :
    { camera : Camera3d Meters Coordinates.World
    , screenRectangle : Rectangle2d Pixels Coordinates.Screen
    , point : Point2d Pixels Coordinates.Screen
    , index : Int
    , debugFlags : DebugFlags
    }
    -> Potential
    -> Potential
editControlPoint cfg (Potential potential) =
    let
        newControlPoints : List Control
        newControlPoints =
            List.indexedMap
                (\i control ->
                    if i == cfg.index then
                        case control of
                            Fixed _ ->
                                control

                            Selected _ ->
                                control
                        -- MovingGlobal editingControl ->
                        --     let
                        --         maybeIntersectionPoint =
                        --             editingControl.point
                        --                 |> Camera3d.ray cfg.camera cfg.screenRectangle
                        --                 |> Axis3d.intersectionWithPlane editingControl.plane
                        --     in
                        --     case maybeIntersectionPoint of
                        --         Nothing ->
                        --             control
                        --         Just intersectionPoint ->
                        --             let
                        --                 axis =
                        --                     Axis3d.through (Frame3d.originPoint editingControl.editingFrame) editingControl.direction
                        --                 oldCenter =
                        --                     Frame3d.originPoint editingControl.editingFrame
                        --                 newEditingFrame =
                        --                     Point3d.projectOntoAxis axis intersectionPoint
                        --                         |> Direction3d.from oldCenter
                        --                         |> Maybe.map
                        --                             (\dir ->
                        --                                 control
                        --                                     |> controlToFrame
                        --                                     |> Frame3d.translateIn
                        --                                         dir
                        --                                         (Point3d.distanceFrom oldCenter intersectionPoint)
                        --                             )
                        --             in
                        --             case newEditingFrame of
                        --                 Nothing ->
                        --                     control
                        --                 Just newFrame ->
                        --                     MovingGlobal { editingControl | editingFrame = newFrame }
                        -- RotatingLocal editingControl ->
                        --     let
                        --         maybeIntersectionPoint =
                        --             cfg.point
                        --                 |> Camera3d.ray cfg.camera cfg.screenRectangle
                        --                 |> Axis3d.intersectionWithPlane editingControl.plane
                        --         -- oldCP =
                        --         --     previousControlPoint
                        --         --         |> Camera3d.ray camera screenRectangle
                        --         --         |> Axis3d.intersectionWithPlane movingControlPoint.plane
                        --     in
                        --     case maybeIntersectionPoint of
                        --         Nothing ->
                        --             control
                        --         Just intersectionPoint ->
                        --             let
                        --                 angle =
                        --                     Point3d.distanceFrom (Frame3d.originPoint editingControl.originalFrame) intersectionPoint
                        --                         |> Length.inMeters
                        --                         -- |> (*) 2
                        --                         -- |> Debug.log "angle"
                        --                         |> Angle.degrees
                        --             in
                        --             RotatingLocal
                        --                 { editingControl
                        --                     | editingFrame =
                        --                         Frame3d.rotateAround
                        --                             editingControl.rotationAxis
                        --                             angle
                        --                             editingControl.originalFrame
                        --                 }

                    else
                        control
                )
                potential.controlPoints
    in
    Potential
        { potential
            | controlPoints = newControlPoints
            , geometry = createTrackGeometry potential.knots (Just cfg.debugFlags) potential.shape newControlPoints
        }


newDebugFlags : DebugFlags -> Potential -> Potential
newDebugFlags debugFlags (Potential track) =
    Potential
        { track
            | geometry = createTrackGeometry track.knots (Just debugFlags) track.shape track.controlPoints
        }


length : Potential -> Result LengthError Length
length (Potential potential) =
    lengthInternal potential


lengthInternal : { a | controlPoints : List Control, knots : List Float } -> Result LengthError Length
lengthInternal { controlPoints, knots } =
    controlPoints
        |> List.map (controlToFrame >> Frame3d.originPoint)
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


type Msg
    = ControlPointSelected Int
    | ControlPointDeselected Int
    | ContrlPointArmSelected ActiveControlPoint
    | ControlPointArmDeselected Int
    | PointerMoved Int (Point2d Pixels Coordinates.Screen)


type alias Effect =
    Never


update : { camera : Camera3d Meters Coordinates.World, debugFlags : DebugFlags } -> Msg -> Potential -> Update Potential Msg Effect
update cfg msg (Potential potential) =
    case msg of
        ControlPointSelected index ->
            { potential
                | controlPoints =
                    potential.controlPoints
                        |> List.indexedMap
                            (\i control ->
                                if i == index then
                                    case control of
                                        Selected frame ->
                                            control

                                        Fixed frame ->
                                            Selected frame

                                else
                                    control
                            )
            }
                |> Potential
                |> Update.save

        ControlPointDeselected index ->
            { potential
                | controlPoints =
                    potential.controlPoints
                        |> List.indexedMap
                            (\i control ->
                                if i == index then
                                    case control of
                                        Fixed _ ->
                                            control

                                        Selected frame ->
                                            Fixed frame

                                else
                                    control
                            )
            }
                |> Potential
                |> Update.save

        ContrlPointArmSelected armDetails ->
            potential
                |> Potential
                |> Update.save

        ControlPointArmDeselected index ->
            potential
                |> Potential
                |> Update.save

        PointerMoved index point ->
            let
                viewSize =
                    { width = 800
                    , height = 600
                    }

                screenRectangle : Rectangle2d Pixels Coordinates.Screen
                screenRectangle =
                    Point2d.pixels viewSize.width 0
                        |> Rectangle2d.from (Point2d.pixels 0 viewSize.height)

                -- newMovingControlPoint =
                --     { movingControlPoint | point = point }
            in
            -- case potential.editMode of
            --     ( Move, Global ) ->
            --         editControlPoint
            --             { camera = cfg.camera
            --             , screenRectangle = screenRectangle
            --             , point = point
            --             , index = index
            --             , debugFlags = cfg.debugFlags
            --             }
            --             (Potential potential)
            --     ( Move, Local ) ->
            --         Debug.todo ""
            --     ( Rotate, Global ) ->
            --         Debug.todo ""
            --     ( Rotate, Local ) ->
            editControlPoint
                { camera = cfg.camera
                , screenRectangle = screenRectangle
                , point = point
                , index = index
                , debugFlags = cfg.debugFlags
                }
                (Potential potential)
                |> Update.save


viewControlPoints :
    { viewSize : { width : Float, height : Float }
    , camera : Camera3d Meters Coordinates.World
    , movingControlPoint : Maybe ActiveControlPoint
    }
    -> Potential
    -> Html Msg
viewControlPoints { viewSize, camera, movingControlPoint } (Potential track) =
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
                    (\control ->
                        { center =
                            control
                                |> controlToFrame
                                |> Frame3d.originPoint
                                |> Point3d.Projection.toScreenSpace camera screenRectangle
                        , point = Frame3d.originPoint (controlToFrame control)
                        , xSegment =
                            LineSegment2d.from
                                (control
                                    |> controlToFrame
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (control
                                        |> controlToFrame
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveX (Length.meters 1)
                                    )
                                )
                        , ySegment =
                            LineSegment2d.from
                                (control
                                    |> controlToFrame
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (control
                                        |> controlToFrame
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveY (Length.meters 1)
                                    )
                                )
                        , zSegment =
                            LineSegment2d.from
                                (control
                                    |> controlToFrame
                                    |> Frame3d.originPoint
                                    |> Point3d.Projection.toScreenSpace camera screenRectangle
                                )
                                (Point3d.Projection.toScreenSpace camera
                                    screenRectangle
                                    (control
                                        |> controlToFrame
                                        |> Frame3d.originPoint
                                        |> Point3d.translateIn Direction3d.positiveZ (Length.meters 1)
                                    )
                                )
                        }
                    )

        viewControlPointMoveArm :
            String
            -> Int
            -> Plane3d Meters Coordinates.World
            -> Direction3d Coordinates.World
            -> Axis3d Meters Coordinates.World
            -> LineSegment2d Pixels Coordinates.Screen
            -> Svg Msg
        viewControlPointMoveArm color index plane dir rotAxis segment =
            Geometry.Svg.lineSegment2d
                ([ Svg.Attributes.stroke color
                 , Svg.Attributes.strokeWidth "4"
                 , Svg.Attributes.class "track-editor-control-point-dir"

                 -- TODO
                 -- ([ Svg.Attributes.pointerEvents "all"
                 , Svg.Events.on "pointerdown"
                    (decodePointerDown
                        (\pointerId point ->
                            ContrlPointArmSelected
                                { pointerId = pointerId
                                , index = index
                                , point = point
                                , direction = dir
                                , plane = plane
                                , rotationAxis = rotAxis
                                }
                        )
                    )
                 , Svg.Events.on "pointerup" (decodePointerUp (ControlPointDeselected index))

                 -- , Svg.Events.on "keydown" (decodeKeyDown index)
                 -- ]
                 -- )
                 ]
                    ++ (case movingControlPoint of
                            Just details ->
                                if details.index == index then
                                    [ Svg.Attributes.style "cursor: grab"
                                    , Html.Attributes.property "___capturePointer" details.pointerId
                                    , Svg.Events.on "pointermove" (decodePointerMove PointerMoved index)
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
                        [ viewControlPointMoveArm
                            "red"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveZ)
                            Direction3d.positiveX
                            (Direction3d.positiveX |> Axis3d.through controlPoint.point)
                            controlPoint.xSegment
                        , viewControlPointMoveArm
                            "green"
                            index
                            (Plane3d.through controlPoint.point Direction3d.positiveZ)
                            Direction3d.positiveY
                            (Direction3d.positiveY |> Axis3d.through controlPoint.point)
                            controlPoint.ySegment
                        , viewControlPointMoveArm
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
                            , Svg.Events.on "pointerdown" (decodePointerDown (\_ _ -> ControlPointSelected index))
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


decodePointerDown : (PointerId -> Point2d Pixels Coordinates.Screen -> msg) -> Json.Decode.Decoder msg
decodePointerDown handler =
    Json.Decode.map3 (\pointerId x y -> handler pointerId (Point2d.pixels x y))
        (Json.Decode.field "pointerId" Json.Decode.value)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


type alias PointerId =
    Json.Decode.Value


decodePointerUp : msg -> Json.Decode.Decoder msg
decodePointerUp handler =
    Json.Decode.succeed handler


decodePointerMove : (Int -> Point2d Pixels Coordinates.Screen -> msg) -> Int -> Json.Decode.Decoder msg
decodePointerMove handler index =
    Json.Decode.map2 (\x y -> handler index (Point2d.pixels x y))
        (Json.Decode.at [ "clientX" ] Json.Decode.float)
        (Json.Decode.at [ "clientY" ] Json.Decode.float)


view : Potential -> Scene3d.Entity Coordinates.World
view (Potential track) =
    track.geometry


editMode : Potential -> ( EditType, Scope )
editMode (Potential track) =
    track.editMode


setEditMode : ( EditType, Scope ) -> Potential -> Potential
setEditMode editMode_ (Potential track) =
    Potential { track | editMode = editMode_ }
