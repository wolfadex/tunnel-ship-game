module Race exposing (Direction(..), Event(..), Flags, Model, Msg(..), RadiansPerSecond, RadiansPerSecondSquared, RotationAcceleration, RotationSpeed, Ship, subscriptions, update, view)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser.Events
import Camera3d
import Color
import Coordinates
import CubicSpline3d
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment3d
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Random
import Scene3d
import Scene3d.Material
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d
import Speed exposing (Speed)
import Time
import Track exposing (Track)
import Update exposing (Update)
import Viewpoint3d


type alias Model =
    { ship : Ship

    -- , otherShips : List Ship
    , seed : Random.Seed
    , keysDown : Set String
    , track : Track
    , lastTickTime : Time.Posix
    }


type alias Ship =
    { geometry : Block3d Meters Coordinates.World
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


initShip : CubicSpline3d.Nondegenerate Meters Coordinates.World -> Shape Coordinates.World -> Ship
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
    , track = Track.init initialShape Nothing
    , lastTickTime = Time.millisToPosix (round timeNow)
    }
        |> Update.save


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
    = CounterClockwise


update : { msg : Msg, model : Model, toMsg : Msg -> msg, toModel : Model -> model } -> Update model msg
update { msg, model, toMsg, toModel } =
    Update.map toModel toMsg <|
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
                    Track.length model.track
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


view : Model -> List (Html Msg)
view model =
    let
        ( focalPoint, _ ) =
            model.ship.distance
                |> Track.sample model.track

        ( followPoint, _ ) =
            model.ship.distance
                |> Quantity.minus (Length.meters 4)
                |> Track.sample model.track

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
            [ Track.view model.track
            , viewShip focalPoint model.track model.ship
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


viewShip : Point3d Meters Coordinates.World -> Track -> Ship -> Scene3d.Entity Coordinates.World
viewShip focalPoint track ship =
    let
        ( center, normal ) =
            Track.sample track ship.distance

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
