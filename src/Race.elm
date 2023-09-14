module Race exposing
    ( Direction(..)
    , Effect
    , Event(..)
    , Flags
    , Model
    , Msg(..)
    , RadiansPerSecond
    , RadiansPerSecondSquared
    , RotationAcceleration
    , RotationSpeed
    , Ship
    , init
    , subscriptions
    , update
    , view
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser.Events
import Camera3d
import Color
import Coordinates
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
import SketchPlane3d
import Speed exposing (Speed)
import Task
import Time
import Track exposing (Track)
import Update exposing (Update)
import Viewpoint3d


type Model
    = Loading Track
    | Loaded LoadedModel


type alias LoadedModel =
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


initShip : Ship
initShip =
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
    Track


init : Flags -> Update Model Msg Effect
init track =
    Loading track
        |> Update.save
        |> Update.withCmd (Time.now |> Task.perform CurrentTimeReceived)


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
    | CurrentTimeReceived Time.Posix


type alias Effect =
    Never


type Event
    = KeyDown String
    | KeyUp String


type Direction
    = CounterClockwise


update : Msg -> Model -> Update Model Msg Effect
update msg model =
    case msg of
        CurrentTimeReceived timeNow ->
            case model of
                Loading track ->
                    Loaded
                        { ship = initShip

                        -- , otherShips =
                        -- [ initShip
                        -- , initShip
                        -- , initShip
                        -- ]
                        , seed = Random.initialSeed 0
                        , keysDown = Set.empty
                        , track = track
                        , lastTickTime = timeNow
                        }
                        |> Update.save

                Loaded _ ->
                    model
                        |> Update.save

        AnimationFrame timestamp ->
            case model of
                Loading _ ->
                    model
                        |> Update.save

                Loaded m ->
                    { m | lastTickTime = timestamp }
                        |> tick (timeDelta m timestamp)

        Event timestamp event ->
            case model of
                Loading _ ->
                    model
                        |> Update.save

                Loaded m ->
                    { m | lastTickTime = timestamp }
                        |> applyEvent event
                        |> tick (timeDelta m timestamp)


timeDelta : LoadedModel -> Time.Posix -> Duration
timeDelta model timestamp =
    (Time.posixToMillis timestamp - Time.posixToMillis model.lastTickTime)
        |> toFloat
        |> Duration.milliseconds


applyEvent : Event -> LoadedModel -> LoadedModel
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


tick : Duration -> LoadedModel -> Update Model Msg Effect
tick deltaTime model =
    model
        |> Update.save
        |> rotateShip deltaTime
        -- |> moveEnemies deltaTime
        |> moveShip deltaTime
        |> Update.mapModel Loaded


moveShip : Duration -> Update LoadedModel Msg Effect -> Update LoadedModel Msg Effect
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


rotateShip : Duration -> Update LoadedModel Msg Effect -> Update LoadedModel Msg Effect
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
    case model of
        Loading _ ->
            [ Html.text "loading..." ]

        Loaded m ->
            viewLoaded m


viewLoaded : LoadedModel -> List (Html Msg)
viewLoaded model =
    let
        ( focalPoint, _ ) =
            Track.sampleTrackAt model.ship.distance model.track

        ( followPoint, frame ) =
            Track.sampleTrackAt
                (model.ship.distance
                    |> Quantity.minus (Length.meters 4)
                )
                model.track

        upDir =
            frame
                |> Frame3d.rotateAround (Frame3d.xAxis frame) model.ship.rotation
                |> Frame3d.yDirection
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
        ( center, frame ) =
            Track.sampleTrackAt ship.distance track

        shipFinal =
            ship.geometry
                |> Block3d.placeIn frame
                |> Block3d.translateIn (Frame3d.zDirection frame) (Length.meters -0.75)
                |> Block3d.rotateAround
                    (frame
                        |> Frame3d.xDirection
                        |> Axis3d.through center
                    )
                    ship.rotation
    in
    Scene3d.group
        [ shipFinal
            |> Scene3d.block (Scene3d.Material.matte Color.green)
        , LineSegment3d.from
            (Block3d.centerPoint shipFinal)
            (shipFinal
                |> Block3d.centerPoint
                |> Point3d.translateIn (Frame3d.xDirection frame) (Length.meters 1)
            )
            |> Scene3d.lineSegment (Scene3d.Material.color Color.red)
        ]
