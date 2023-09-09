module Main exposing
    ( Direction(..)
    , Enemy
    , Laser
    , Model
    , Msg(..)
    , RotatingDetails
    , Ship
    , WorldCoordinates(..)
    , main
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d
import Color
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction2d
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame2d
import Frame3d
import Html
import Html.Attributes
import Interval
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import Pixels
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d
import Polyline3d
import Quantity exposing (Quantity)
import Random
import Rectangle2d
import Rectangle3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d exposing (SketchPlane3d)
import Speed exposing (Speed)
import Time
import Update exposing (Update)
import Util.Debug
import Util.Function
import Util.List
import Util.Maybe
import Util.Random
import Vector3d exposing (Vector3d)
import Viewpoint3d


main : Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                init flags
                    |> Update.complete
        , view = view
        , update =
            \msg model ->
                update msg model
                    |> Update.complete
        , subscriptions = subscriptions
        }


type alias Model =
    { ship : Ship

    -- , otherShips : List Ship
    , seed : Random.Seed
    , keysDown : Set String
    , track : Track
    , lastTickTime : Time.Posix
    }


type alias Track =
    { shape : Shape WorldCoordinates
    , path : CubicSpline3d.Nondegenerate Meters WorldCoordinates
    , geometry : Scene3d.Entity WorldCoordinates
    }


type alias Laser =
    Point3d Meters WorldCoordinates


type alias Enemy =
    { location : Point3d Meters WorldCoordinates
    }


type alias Ship =
    { geometry : Block3d Meters WorldCoordinates
    , speed : Speed
    , acceleration : Acceleration
    , distance : Length
    , rotationSpeed : RotationSpeed
    , rotationAcceleration : RotationAcceleration
    , rotation : Angle
    }


type alias RotationSpeed =
    Quantity Float RadiansPerSecond


type alias RadiansPerSecond =
    Quantity.Rate Angle.Radians Duration.Seconds


type alias RadiansPerSecondSquared =
    Quantity.Rate RotationSpeed Duration.Seconds


type alias RotationAcceleration =
    Quantity Float RadiansPerSecondSquared


type alias RotatingDetails =
    { travelTime : Float
    , direction : Direction
    , shootOnComplete : Bool
    }


type WorldCoordinates
    = WorldCoordinates Never


initShip : CubicSpline3d.Nondegenerate Meters WorldCoordinates -> Shape WorldCoordinates -> Ship
initShip path shape =
    { geometry =
        Block3d.centeredOn Frame3d.atOrigin
            ( Length.meters 0.5
            , Length.meters 0.125
            , Length.meters 1
            )
    , speed = Speed.kilometersPerHour 0
    , acceleration = Acceleration.metersPerSecondSquared 40
    , distance = Length.meters 0
    , rotationSpeed = Quantity.rate (Angle.degrees 50) (Duration.seconds 1)
    , rotationAcceleration = Quantity.Quantity 1
    , rotation = Angle.degrees 0
    }


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
    { ship =
        initShip path initialShape

    -- , otherShips =
    -- [ initialShape
    --     |> Polygon2d.vertices
    --     |> List.drop 1
    --     |> List.take 2
    --     |> to2Points
    --     |> initShip
    -- , initialShape
    --     |> Polygon2d.vertices
    --     |> List.drop 3
    --     |> List.take 2
    --     |> to2Points
    --     |> initShip
    -- , initialShape
    --     |> Polygon2d.vertices
    --     |> List.drop 5
    --     |> List.take 2
    --     |> to2Points
    --     |> initShip
    -- ]
    , seed = Random.initialSeed 0
    , keysDown = Set.empty
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
    , lastTickTime = Time.millisToPosix (round timeNow)
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
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onKeyDown decodeKeyDown
        , Browser.Events.onKeyUp decodeKeyUp
        ]


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map KeyDown
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
            { model | lastTickTime = timestamp }
                |> applyEvent event
                |> tick (timeDelta model timestamp)


timeDelta : Model -> Time.Posix -> Duration
timeDelta model timestamp =
    (Time.posixToMillis timestamp - Time.posixToMillis model.lastTickTime)
        |> toFloat
        |> Duration.milliseconds


applyEvent : Event -> Model -> Model
applyEvent event model =
    case event of
        KeyDown key ->
            { model
                | keysDown = Set.insert key model.keysDown
            }

        KeyUp key ->
            { model
                | keysDown = Set.remove key model.keysDown
            }


tick : Duration -> Model -> Update Model Msg
tick deltaTime model =
    model
        |> Update.save
        |> rotateShip deltaTime
        -- |> moveEnemies deltaTime
        |> moveShip deltaTime


moveShip : Duration -> Update Model Msg -> Update Model Msg
moveShip deltaTime =
    Update.mapModel
        (\model ->
            let
                ship =
                    model.ship

                distance =
                    ship.speed
                        |> Quantity.for deltaTime
                        |> Quantity.plus ship.distance

                lengthOfTrack =
                    CubicSpline3d.arcLengthParameterized
                        { maxError = Length.meters 0.01 }
                        model.track.path
                        |> CubicSpline3d.arcLength
            in
            { model
                | ship =
                    { ship
                        | speed =
                            ship.acceleration
                                |> Quantity.for deltaTime
                                |> Quantity.plus ship.speed
                                |> Quantity.min (Speed.kilometersPerHour 10)
                        , distance =
                            if distance |> Quantity.greaterThan lengthOfTrack then
                                Length.kilometers 0

                            else
                                distance
                    }
            }
        )


rotateShip : Duration -> Update Model Msg -> Update Model Msg
rotateShip deltaTime =
    Update.mapModel
        (\model ->
            let
                ship =
                    model.ship
            in
            { model
                | ship =
                    { ship
                        | rotation =
                            if Set.member "ArrowRight" model.keysDown then
                                ship.rotationSpeed
                                    |> Quantity.for deltaTime
                                    |> Quantity.difference ship.rotation

                            else if Set.member "ArrowLeft" model.keysDown then
                                ship.rotationSpeed
                                    |> Quantity.for deltaTime
                                    |> Quantity.plus ship.rotation

                            else
                                ship.rotation
                    }
            }
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Tunnle Rocket"
    , body =
        let
            arcLengthParam =
                CubicSpline3d.arcLengthParameterized
                    { maxError = Length.meters 0.01 }
                    model.track.path

            ( focalPoint, _ ) =
                model.ship.distance
                    |> CubicSpline3d.sampleAlong arcLengthParam

            ( followPoint, _ ) =
                model.ship.distance
                    |> Quantity.minus (Length.meters 4)
                    |> CubicSpline3d.sampleAlong arcLengthParam

            sketchPlane =
                SketchPlane3d.through followPoint
                    (Direction3d.from followPoint focalPoint
                        |> Maybe.withDefault Direction3d.positiveZ
                    )

            upDir =
                sketchPlane
                    |> SketchPlane3d.rotateAround (SketchPlane3d.normalAxis sketchPlane) model.ship.rotation
                    |> SketchPlane3d.yDirection
                    |> Direction3d.reverse
        in
        [ Scene3d.cloudy
            { dimensions = ( Pixels.int 800, Pixels.int 600 )
            , upDirection = Direction3d.negativeY
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { eyePoint = followPoint
                            , focalPoint = focalPoint
                            , upDirection = upDir
                            }
                    , verticalFieldOfView = Angle.degrees 30
                    }
            , clipDepth = Length.millimeters 0.1
            , background = Scene3d.backgroundColor Color.black
            , entities =
                [ model.track.geometry
                , viewShip focalPoint model.track.path model.ship
                ]
            }

        -- DEBUG
        , Html.div
            []
            [ Html.span [ Html.Attributes.class "score" ]
                [ ((model.ship.speed
                        |> Speed.inKilometersPerHour
                        |> String.fromFloat
                   )
                    ++ "km/h"
                  )
                    |> Html.text
                ]
            ]
        , Html.div
            []
            [ Html.span [ Html.Attributes.class "score" ]
                [ ((model.ship.distance
                        |> Length.inKilometers
                        |> String.fromFloat
                   )
                    ++ "km"
                  )
                    |> Html.text
                ]
            ]
        , Html.progress
            [ model.ship.distance
                |> Length.inKilometers
                |> String.fromFloat
                |> Html.Attributes.value
            , 100
                |> String.fromFloat
                |> Html.Attributes.max
            ]
            []
        ]
    }


viewShip : Point3d Meters WorldCoordinates -> CubicSpline3d.Nondegenerate Meters WorldCoordinates -> Ship -> Scene3d.Entity WorldCoordinates
viewShip focalPoint path ship =
    let
        arcLengthParam =
            CubicSpline3d.arcLengthParameterized
                { maxError = Length.meters 0.01 }
                path

        ( center, normal ) =
            CubicSpline3d.sampleAlong arcLengthParam ship.distance

        sketchPlane =
            SketchPlane3d.through center normal

        downDir =
            sketchPlane
                |> SketchPlane3d.yDirection

        shipFinal =
            ship.geometry
                |> Block3d.placeIn (SketchPlane3d.toFrame sketchPlane)
                |> Block3d.translateIn downDir (Length.meters 0.75)
                |> Block3d.rotateAround (Axis3d.through center normal) ship.rotation
    in
    Scene3d.group
        [ shipFinal
            |> Scene3d.block (Scene3d.Material.matte Color.green)
        , LineSegment3d.from
            (Block3d.centerPoint shipFinal)
            (Block3d.centerPoint shipFinal
                |> Point3d.translateIn normal (Length.meters 1)
            )
            |> Scene3d.lineSegment (Scene3d.Material.color Color.red)
        ]


makeRailThingy : Cylinder3d Meters coordinates -> Scene3d.Entity coordinates
makeRailThingy =
    Scene3d.cylinder (Scene3d.Material.matte Color.green)


laserToGeometry : Laser -> Cylinder3d Meters WorldCoordinates
laserToGeometry laser =
    Cylinder3d.centeredOn laser
        Direction3d.positiveY
        { radius = Length.meters 0.03125
        , length = Length.meters 1
        }


viewLaser : Laser -> Scene3d.Entity WorldCoordinates
viewLaser laser =
    laser
        |> laserToGeometry
        |> Scene3d.cylinder (Scene3d.Material.matte Color.lightBlue)


enemyToGeometry : Enemy -> Cylinder3d Meters WorldCoordinates
enemyToGeometry enemy =
    let
        directionTowardsCenter =
            enemy.location
                |> Point3d.unwrap
                |> (\p -> Point3d.unsafe { p | x = 0, z = 0 })
                |> Direction3d.from enemy.location
                |> Maybe.withDefault Direction3d.positiveX

        height =
            0.5
    in
    Cylinder3d.centeredOn enemy.location
        directionTowardsCenter
        { radius = Length.meters 0.25
        , length = Length.meters height
        }
        |> Cylinder3d.translateIn directionTowardsCenter (Length.meters (height / 2))


viewEnemy : Enemy -> Scene3d.Entity WorldCoordinates
viewEnemy enemy =
    enemy
        |> enemyToGeometry
        |> Scene3d.cylinder (Scene3d.Material.matte Color.red)
