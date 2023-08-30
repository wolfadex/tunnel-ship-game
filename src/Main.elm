module Main exposing (..)

import Angle
import Block3d exposing (Block3d)
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d
import Frame3d
import Html exposing (Html)
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
import Polygon2d exposing (Polygon2d)
import Quantity
import Random
import Rectangle2d
import Rectangle3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Shape exposing (Shape)
import SketchPlane3d
import Sphere3d
import Time
import Util.Debug
import Util.Function
import Util.List
import Util.Maybe
import Util.Random
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { ship : Ship
    , lasers : List Laser
    , enemies : List Enemy
    , nextEnemySpawn : Float
    , seed : Random.Seed
    , score : Int
    , shape : Shape WorldCoordinates
    }


type alias Laser =
    Point3d Meters WorldCoordinates


type alias Enemy =
    { location : Point3d Meters WorldCoordinates
    }


type alias Ship =
    { rocket1 : Cylinder3d Meters WorldCoordinates
    , rocket1Index : Int
    , rocket2 : Cylinder3d Meters WorldCoordinates
    , rocket2Index : Int
    , body : Block3d Meters WorldCoordinates
    , moving : Maybe MovingDetails
    , upIsNormal : Bool
    }


type alias MovingDetails =
    { travelTime : Float
    , direction : Direction
    , shootOnComplete : Bool
    }


type WorldCoordinates
    = WorldCoordinates Never


initialShip : Polygon2d Meters coordinates -> Ship
initialShip shape =
    shape
        |> Polygon2d.vertices
        |> List.take 2
        |> to2Points
        |> (\( a, b ) ->
                let
                    makeRailRocket : Point2d Meters coordinates -> Cylinder3d Meters WorldCoordinates
                    makeRailRocket point =
                        Cylinder3d.centeredOn
                            (Point3d.on
                                (SketchPlane3d.xz
                                    |> SketchPlane3d.translateIn Direction3d.positiveY (Length.meters 0)
                                )
                                point
                            )
                            Direction3d.positiveY
                            { radius = Length.meters 0.0625
                            , length = Length.meters 1
                            }
                in
                { rocket1 = makeRailRocket a
                , rocket1Index = 0
                , rocket2 = makeRailRocket b
                , rocket2Index = 1
                , body =
                    let
                        frame2d =
                            LineSegment2d.from a b
                                |> LineSegment2d.midpoint
                                |> Frame2d.withXDirection
                                    (Direction2d.from a b
                                        |> Maybe.withDefault Direction2d.positiveX
                                    )

                        frame3d =
                            Rectangle2d.centeredOn frame2d ( Length.meters 0.5, Length.meters 0.25 )
                                |> Rectangle3d.on
                                    (SketchPlane3d.xz
                                        |> SketchPlane3d.translateIn Direction3d.positiveY (Length.meters 0)
                                    )
                                |> Rectangle3d.xAxis
                                |> Frame3d.fromXAxis
                    in
                    Block3d.centeredOn frame3d
                        ( Length.meters 0.25
                        , Length.meters 0.125
                        , Length.meters 1
                        )
                        |> Block3d.translateIn
                            (Frame3d.yDirection frame3d)
                            (Length.meters -0.125)
                , moving = Nothing
                , upIsNormal = True
                }
           )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialShape =
            Shape.new 7
    in
    ( { ship = initialShip initialShape
      , lasers = []
      , enemies = []
      , nextEnemySpawn = enemySpawnRate
      , seed = Random.initialSeed 0
      , score = 0
      , shape = initialShape
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown decodeKeyDown
        ]


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.andThen
        (\key ->
            case key of
                "ArrowLeft" ->
                    Json.Decode.succeed (Move Clockwise)

                "ArrowRight" ->
                    Json.Decode.succeed (Move CounterClockwise)

                " " ->
                    Json.Decode.succeed Shoot

                _ ->
                    Json.Decode.fail "Unsupported key"
        )
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = Tick Float
    | Move Direction
    | Shoot


type Direction
    = Clockwise
    | CounterClockwise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( model
                |> moveShip deltaMs
                |> moveLasers deltaMs
                |> spawnEnemy deltaMs
                |> killEnemies
                |> moveEnemies deltaMs
            , Cmd.none
            )

        Move direction ->
            let
                ship =
                    model.ship
            in
            ( { model
                | ship =
                    { ship
                        | moving =
                            case ship.moving of
                                Nothing ->
                                    Just
                                        { travelTime = rotateAnimationTime
                                        , direction = direction
                                        , shootOnComplete = False
                                        }

                                Just _ ->
                                    ship.moving
                    }
              }
            , Cmd.none
            )

        Shoot ->
            ( case model.ship.moving of
                Nothing ->
                    shootLaser model

                Just moving ->
                    let
                        ship =
                            model.ship
                    in
                    { model
                        | ship =
                            { ship
                                | moving =
                                    Just { moving | shootOnComplete = True }
                            }
                    }
            , Cmd.none
            )


enemySpawnRate =
    2000


spawnEnemy : Float -> Model -> Model
spawnEnemy deltaMs model =
    let
        nextEnemySpawn =
            model.nextEnemySpawn - deltaMs
    in
    if nextEnemySpawn <= 0 then
        let
            ( spawnPoint, seed ) =
                Random.step
                    (model.shape
                        |> Polygon2d.edges
                        |> List.map
                            (LineSegment3d.on
                                (SketchPlane3d.xz
                                    |> SketchPlane3d.translateIn Direction3d.positiveY (Length.meters 30)
                                )
                                >> LineSegment3d.midpoint
                            )
                        |> Util.Random.fromList
                        |> Random.map (Maybe.withDefault Point3d.origin)
                    )
                    model.seed
        in
        { model
            | nextEnemySpawn = nextEnemySpawn + enemySpawnRate
            , enemies =
                { location =
                    spawnPoint
                        |> Point3d.translateIn Direction3d.positiveY (Length.meters 30)
                }
                    :: model.enemies
            , seed = seed
        }

    else
        { model
            | nextEnemySpawn = nextEnemySpawn
        }


moveEnemies : Float -> Model -> Model
moveEnemies deltaMs model =
    { model
        | enemies =
            List.filterMap (moveEnemy deltaMs) model.enemies
    }


moveEnemy : Float -> Enemy -> Maybe Enemy
moveEnemy deltaMs enemy =
    let
        newLocation =
            enemy.location
                |> Point3d.translateIn Direction3d.negativeY (Length.meters (deltaMs * 0.01))
    in
    if Point3d.yCoordinate newLocation |> Quantity.lessThan (Length.meters -10) then
        Nothing

    else
        Just
            { enemy
                | location = newLocation
            }


killEnemies : Model -> Model
killEnemies model =
    let
        remaining =
            checkLaserCollisions { enemies = model.enemies, lasers = model.lasers }
    in
    { model
        | enemies = remaining.enemies
        , lasers = remaining.lasers

        -- TODO: Add explosion animation
        -- , enemiesToExplode = remaining.destroyedEnemies
        , score =
            remaining.destroyedEnemies
                |> List.map
                    (\enemy ->
                        Point3d.yCoordinate enemy.location
                            |> Length.inMeters
                            |> round
                    )
                |> List.sum
                |> (+) model.score
    }


checkLaserCollisions :
    { enemies : List Enemy
    , lasers : List Laser
    }
    ->
        { enemies : List Enemy
        , lasers : List Laser
        , destroyedEnemies : List Enemy
        }
checkLaserCollisions input =
    let
        helper :
            List Laser
            -> { enemies : List Enemy, lasers : List Laser, destroyedEnemies : List Enemy }
            -> { enemies : List Enemy, lasers : List Laser, destroyedEnemies : List Enemy }
        helper lasers output =
            case lasers of
                [] ->
                    output

                nextLaser :: remainingLasers ->
                    let
                        ( maybeEnemy, restEnemies ) =
                            Util.List.extractIf
                                (\enemy ->
                                    BoundingBox3d.intersects
                                        (Sphere3d.atPoint enemy.location
                                            (Length.meters 0.5)
                                            |> Sphere3d.boundingBox
                                        )
                                        (Cylinder3d.centeredOn nextLaser
                                            Direction3d.positiveY
                                            { radius = Length.meters 0.03125
                                            , length = Length.meters 1
                                            }
                                            |> Cylinder3d.boundingBox
                                        )
                                )
                                output.enemies
                    in
                    case maybeEnemy of
                        Nothing ->
                            helper remainingLasers
                                { output
                                    | lasers = nextLaser :: output.lasers
                                }

                        Just enemy ->
                            helper remainingLasers
                                { output
                                    | enemies = restEnemies
                                    , destroyedEnemies = enemy :: output.destroyedEnemies
                                }
    in
    helper
        input.lasers
        { enemies = input.enemies
        , lasers = []
        , destroyedEnemies = []
        }


shootLaser : Model -> Model
shootLaser model =
    { model
        | lasers =
            Block3d.centerPoint model.ship.body :: model.lasers
    }


moveShip : Float -> Model -> Model
moveShip deltaMs model =
    case model.ship.moving of
        Nothing ->
            model

        Just ({ travelTime, direction } as moving) ->
            let
                ship : Ship
                ship =
                    model.ship

                remainingTime : Float
                remainingTime =
                    travelTime - deltaMs

                angle =
                    let
                        point1 : Maybe (Point2d Meters WorldCoordinates)
                        point1 =
                            model.shape
                                |> Polygon2d.vertices
                                |> List.Extra.getAt ship.rocket1Index

                        point2 : Maybe (Point2d Meters WorldCoordinates)
                        point2 =
                            model.shape
                                |> Polygon2d.vertices
                                |> List.Extra.getAt ship.rocket2Index

                        point3 : Maybe (Point2d Meters WorldCoordinates)
                        point3 =
                            model.shape
                                |> Polygon2d.vertices
                                |> List.Extra.getAt
                                    (modBy (Shape.sideCount model.shape) <|
                                        case direction of
                                            Clockwise ->
                                                if ship.upIsNormal then
                                                    ship.rocket1Index - 1

                                                else
                                                    ship.rocket2Index - 1

                                            CounterClockwise ->
                                                if ship.upIsNormal then
                                                    ship.rocket2Index + 1

                                                else
                                                    ship.rocket1Index + 1
                                    )
                    in
                    case direction of
                        Clockwise ->
                            if ship.upIsNormal then
                                Maybe.map2 Direction2d.angleFrom
                                    (Util.Maybe.andThen2 Direction2d.from point1 point2)
                                    (Util.Maybe.andThen2 Direction2d.from point1 point3)
                                    |> Maybe.map Quantity.negate

                            else
                                Maybe.map2 Direction2d.angleFrom
                                    (Util.Maybe.andThen2 Direction2d.from point2 point1)
                                    (Util.Maybe.andThen2 Direction2d.from point2 point3)
                                    |> Maybe.map Quantity.negate

                        CounterClockwise ->
                            if ship.upIsNormal then
                                Maybe.map2 Direction2d.angleFrom
                                    (Util.Maybe.andThen2 Direction2d.from point2 point1)
                                    (Util.Maybe.andThen2 Direction2d.from point2 point3)
                                    |> Maybe.map Quantity.negate

                            else
                                Maybe.map2 Direction2d.angleFrom
                                    (Util.Maybe.andThen2 Direction2d.from point1 point3)
                                    (Util.Maybe.andThen2 Direction2d.from point1 point2)

                distanceToRotate =
                    angle
                        |> Maybe.map (Quantity.divideBy rotateAnimationTime)
                        |> Maybe.withDefault (Angle.degrees 0)
                        |> (if remainingTime > 0 then
                                Quantity.multiplyBy deltaMs

                            else
                                Quantity.multiplyBy travelTime
                           )

                newRocket1 =
                    if (ship.upIsNormal && direction == CounterClockwise) || (not ship.upIsNormal && direction == Clockwise) then
                        Cylinder3d.rotateAround
                            (Cylinder3d.axis ship.rocket2)
                            distanceToRotate
                            ship.rocket1

                    else
                        ship.rocket1

                newRocket2 =
                    if (ship.upIsNormal && direction == Clockwise) || (not ship.upIsNormal && direction == CounterClockwise) then
                        Cylinder3d.rotateAround
                            (Cylinder3d.axis ship.rocket1)
                            distanceToRotate
                            ship.rocket2

                    else
                        ship.rocket2

                bodyOffsetInterval =
                    Interval.from -0.125 0.125

                newShip =
                    { ship
                        | rocket1 = newRocket1
                        , rocket2 = newRocket2
                        , body =
                            let
                                a : Point3d Meters WorldCoordinates
                                a =
                                    newRocket1
                                        |> Cylinder3d.centerPoint

                                b : Point3d Meters WorldCoordinates
                                b =
                                    newRocket2
                                        |> Cylinder3d.centerPoint

                                frame3d =
                                    LineSegment3d.from a b
                                        |> LineSegment3d.midpoint
                                        |> Frame3d.withXDirection
                                            (Direction3d.from a b
                                                |> Maybe.withDefault Direction3d.positiveX
                                            )
                            in
                            Block3d.centeredOn frame3d
                                ( Length.meters 0.25
                                , Length.meters 0.125
                                , Length.meters 1
                                )
                                |> Block3d.translateIn
                                    (Frame3d.yDirection frame3d)
                                    (max 0 remainingTime
                                        |> (\x ->
                                                if ship.upIsNormal then
                                                    rotateAnimationTime - x

                                                else
                                                    x
                                           )
                                        |> normalize 0 rotateAnimationTime
                                        |> Interval.interpolate bodyOffsetInterval
                                        |> Length.meters
                                    )
                    }
                        |> shipDoneMoving model.shape moving direction remainingTime
            in
            { model | ship = newShip }
                |> Util.Function.applyIf (moving.shootOnComplete && remainingTime <= 0) shootLaser


shipDoneMoving : Shape WorldCoordinates -> MovingDetails -> Direction -> Float -> Ship -> Ship
shipDoneMoving shape moving direction remainingTime ship =
    if remainingTime > 0 then
        { ship
            | moving =
                Just { moving | travelTime = remainingTime }
        }

    else
        { ship
            | moving = Nothing
            , upIsNormal = not ship.upIsNormal
            , rocket1Index =
                case direction of
                    Clockwise ->
                        if ship.upIsNormal then
                            ship.rocket1Index

                        else
                            (ship.rocket2Index - 1)
                                |> modBy (Shape.sideCount shape)

                    CounterClockwise ->
                        if ship.upIsNormal then
                            (ship.rocket2Index + 1)
                                |> modBy (Shape.sideCount shape)

                        else
                            ship.rocket1Index
            , rocket2Index =
                case direction of
                    Clockwise ->
                        if ship.upIsNormal then
                            (ship.rocket1Index - 1)
                                |> modBy (Shape.sideCount shape)

                        else
                            ship.rocket2Index

                    CounterClockwise ->
                        if ship.upIsNormal then
                            ship.rocket2Index

                        else
                            (ship.rocket1Index + 1)
                                |> modBy (Shape.sideCount shape)
        }


moveLasers : Float -> Model -> Model
moveLasers deltaMs model =
    { model
        | lasers = List.filterMap (moveLaser deltaMs) model.lasers
    }


moveLaser : Float -> Point3d Meters WorldCoordinates -> Maybe (Point3d Meters WorldCoordinates)
moveLaser deltaMs point =
    let
        newPoint =
            Point3d.translateIn Direction3d.positiveY (Length.meters (deltaMs * 0.1)) point
    in
    if Point3d.yCoordinate newPoint |> Quantity.greaterThan (Length.meters 70) then
        Nothing

    else
        Just newPoint


rotateAnimationTime =
    200


normalize : Float -> Float -> Float -> Float
normalize minValue maxValue x =
    (x - minValue) / (maxValue - minValue)


view : Model -> Browser.Document Msg
view model =
    { title = "Elm App"
    , body =
        [ Scene3d.cloudy
            { dimensions = ( Pixels.int 800, Pixels.int 600 )
            , upDirection = Direction3d.negativeY
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { eyePoint = Point3d.meters 0 -5 0
                            , focalPoint = Point3d.origin
                            , upDirection = Direction3d.positiveZ
                            }
                    , verticalFieldOfView = Angle.degrees 30
                    }
            , clipDepth = Length.millimeters 0.1
            , background = Scene3d.backgroundColor Color.black
            , entities =
                [ tunnelRing model.shape (Length.meters 0)
                , tunnelRing model.shape (Length.meters 10)
                , tunnelRing model.shape (Length.meters 20)
                , tunnelRing model.shape (Length.meters 30)
                , [ makeRailThingy model.ship.rocket1
                  , makeRailThingy model.ship.rocket2
                  , Scene3d.block (Scene3d.Material.matte Color.green)
                        model.ship.body
                  ]
                    |> Scene3d.group
                , model.lasers
                    |> List.map viewLaser
                    |> Scene3d.group
                , model.enemies
                    |> List.map viewEnemy
                    |> Scene3d.group
                ]
            }
        , Html.div
            []
            [ Html.span [ Html.Attributes.class "score" ]
                [ Html.text <| "Score " ++ String.fromInt model.score ]
            ]
        ]
    }


makeRailThingy : Cylinder3d Meters coordinates -> Scene3d.Entity coordinates
makeRailThingy =
    Scene3d.cylinder (Scene3d.Material.matte Color.green)


to2Points : List (Point2d Meters coordinates) -> ( Point2d Meters coordinates, Point2d Meters coordinates )
to2Points points =
    case points of
        [ a, b ] ->
            ( a, b )

        _ ->
            ( Point2d.origin, Point2d.origin )


tunnelRing : Polygon2d Meters coordinates -> Length -> Scene3d.Entity coordinates
tunnelRing shape distance =
    shape
        |> Polygon2d.edges
        |> List.map
            (LineSegment3d.on
                (SketchPlane3d.xz
                    |> SketchPlane3d.translateIn Direction3d.positiveY distance
                )
            )
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh
            (Scene3d.Material.color Color.red)


viewLaser : Point3d Meters coordinates -> Scene3d.Entity coordinates
viewLaser point =
    Cylinder3d.centeredOn point
        Direction3d.positiveY
        { radius = Length.meters 0.03125
        , length = Length.meters 1
        }
        |> Scene3d.cylinder (Scene3d.Material.matte Color.blue)


viewEnemy : Enemy -> Scene3d.Entity WorldCoordinates
viewEnemy enemy =
    Sphere3d.atPoint enemy.location
        (Length.meters 0.5)
        |> Scene3d.sphere (Scene3d.Material.matte Color.red)
