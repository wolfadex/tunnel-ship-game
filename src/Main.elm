module Main exposing (Direction(..), Enemy, Laser, Model, MovingDetails, Msg(..), Ship, WorldCoordinates(..), main)

import Angle
import Block3d exposing (Block3d)
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction2d
import Direction3d
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
import Quantity
import Random
import Rectangle2d
import Rectangle3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d
import Update exposing (Update)
import Util.Debug
import Util.Function
import Util.List
import Util.Maybe
import Util.Random
import Viewpoint3d


main : Program () Model Msg
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
    , lasers : List Laser
    , enemies : List Enemy
    , nextEnemySpawn : Float
    , seed : Random.Seed
    , score : Int
    , shape : Shape WorldCoordinates
    , tunnelOffset : Float
    , keysDown : Set String
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


initialShip : Shape coordinates -> Ship
initialShip shape =
    shape
        |> Polygon2d.vertices
        |> (\verts ->
                List.filterMap identity
                    [ List.head verts
                    , verts
                        |> List.reverse
                        |> List.head
                    ]
           )
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
                { rocket1 = makeRailRocket b
                , rocket1Index = 0
                , rocket2 = makeRailRocket a
                , rocket2Index = 1
                , body =
                    let
                        frame2d =
                            LineSegment2d.from b a
                                |> LineSegment2d.midpoint
                                |> Frame2d.withXDirection
                                    (Direction2d.from b a
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


init : () -> Update Model Msg
init () =
    let
        initialShape =
            -- Shape.custom
            Shape.newRegular 7
    in
    { ship = initialShip initialShape
    , lasers = []
    , enemies = []
    , nextEnemySpawn = enemySpawnRate
    , seed = Random.initialSeed 0
    , score = 0
    , shape = initialShape
    , tunnelOffset = 0
    , keysDown = Set.empty
    }
        |> Update.save


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown decodeKeyDown
        , Browser.Events.onKeyUp decodeKeyUp
        ]


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map KeyDown
        (Json.Decode.field "key" Json.Decode.string)


decodeKeyUp : Json.Decode.Decoder Msg
decodeKeyUp =
    Json.Decode.map KeyUp
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | Move Direction
    | Shoot


type Direction
    = Clockwise
    | CounterClockwise


update : Msg -> Model -> Update Model Msg
update msg model =
    case msg of
        Tick deltaMs ->
            model
                |> Update.save
                |> applyKeys
                |> moveShip deltaMs
                |> moveLasers deltaMs
                |> spawnEnemy deltaMs
                |> killEnemies
                |> moveEnemies deltaMs
                |> moveTunnel deltaMs

        KeyDown key ->
            { model
                | keysDown = Set.insert key model.keysDown
            }
                |> Update.save

        KeyUp key ->
            { model
                | keysDown = Set.remove key model.keysDown
            }
                |> Update.save

        Move direction ->
            let
                ship =
                    model.ship
            in
            { model
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
                |> Update.save

        Shoot ->
            case model.ship.moving of
                Nothing ->
                    model
                        |> shootLaser
                        |> Update.save

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
                        |> Update.save


applyKeys : Update Model Msg -> Update Model Msg
applyKeys =
    let
        applyKey model key msg =
            Util.Function.applyIf (Set.member key model.keysDown) msg
    in
    Update.andThen
        (\model ->
            model
                |> Update.save
                |> applyKey model " " (Update.withMsg Shoot)
                |> applyKey model "ArrowLeft" (Update.withMsg (Move Clockwise))
                |> applyKey model "ArrowRight" (Update.withMsg (Move CounterClockwise))
        )


moveTunnel : Float -> Update Model Msg -> Update Model Msg
moveTunnel deltaMs =
    Update.mapModel
        (\model ->
            let
                tunnelOffset =
                    model.tunnelOffset + (deltaMs / enemyMoveSpeed)
            in
            { model
                | tunnelOffset =
                    if tunnelOffset >= tunnelMoveMax then
                        tunnelOffset - tunnelMoveMax

                    else
                        tunnelOffset
            }
        )


tunnelMoveMax =
    10


enemySpawnRate =
    2000


spawnEnemy : Float -> Update Model Msg -> Update Model Msg
spawnEnemy deltaMs =
    Update.mapModel
        (\model ->
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
        )


moveEnemies : Float -> Update Model Msg -> Update Model Msg
moveEnemies deltaMs =
    Update.mapModel
        (\model ->
            { model
                | enemies =
                    List.filterMap (moveEnemy deltaMs) model.enemies
            }
        )


enemyMoveSpeed =
    50


moveEnemy : Float -> Enemy -> Maybe Enemy
moveEnemy deltaMs enemy =
    let
        newLocation =
            enemy.location
                |> Point3d.translateIn Direction3d.negativeY (Length.meters (deltaMs / enemyMoveSpeed))
    in
    if Point3d.yCoordinate newLocation |> Quantity.lessThan (Length.meters -10) then
        Nothing

    else
        Just
            { enemy
                | location = newLocation
            }


killEnemies : Update Model Msg -> Update Model Msg
killEnemies =
    Update.mapModel
        (\model ->
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
        )


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
                                        (enemy
                                            |> enemyToGeometry
                                            |> Cylinder3d.boundingBox
                                        )
                                        (nextLaser
                                            |> laserToGeometry
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


moveShip : Float -> Update Model Msg -> Update Model Msg
moveShip deltaMs =
    Update.mapModel
        (\model ->
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
                                            |> Maybe.map (Util.Debug.logMap Angle.inDegrees "clock norm")

                                    else
                                        Maybe.map2 Direction2d.angleFrom
                                            (Util.Maybe.andThen2 Direction2d.from point2 point1)
                                            (Util.Maybe.andThen2 Direction2d.from point2 point3)
                                            |> Maybe.map Quantity.negate
                                            |> Maybe.map (Util.Debug.logMap Angle.inDegrees "clock ab-norm")

                                CounterClockwise ->
                                    if ship.upIsNormal then
                                        Maybe.map2 Direction2d.angleFrom
                                            (Util.Maybe.andThen2 Direction2d.from point2 point1)
                                            (Util.Maybe.andThen2 Direction2d.from point2 point3)
                                            |> Maybe.map Quantity.negate
                                            |> Maybe.map (Util.Debug.logMap Angle.inDegrees "count-clock norm")

                                    else
                                        Maybe.map2 Direction2d.angleFrom
                                            (Util.Maybe.andThen2 Direction2d.from point1 point3)
                                            (Util.Maybe.andThen2 Direction2d.from point1 point2)
                                            |> Maybe.map (Util.Debug.logMap Angle.inDegrees "count-clock ab-norm")

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

                        newShip =
                            { ship
                                | rocket1 = newRocket1
                                , rocket2 = newRocket2
                                , body =
                                    let
                                        bodyOffsetInterval =
                                            Interval.from -0.125 0.125

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
        )


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


moveLasers : Float -> Update Model Msg -> Update Model Msg
moveLasers deltaMs =
    Update.mapModel
        (\model ->
            { model
                | lasers = List.filterMap (moveLaser deltaMs) model.lasers
            }
        )


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
    150


normalize : Float -> Float -> Float -> Float
normalize minValue maxValue x =
    (x - minValue) / (maxValue - minValue)


view : Model -> Browser.Document Msg
view model =
    { title = "Tunnle Rocket"
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
                [ [ 0, 10, 20, 30, 40, 50, 60 ]
                    |> List.map (\dist -> viewTunnelRing model.shape (Length.meters (dist - model.tunnelOffset)))
                    |> Scene3d.group
                , model.shape
                    |> Polygon2d.vertices
                    |> List.map viewTunnelVertConnectors
                    |> Scene3d.group
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


viewTunnelRing : Shape coordinates -> Length -> Scene3d.Entity coordinates
viewTunnelRing shape distance =
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
            (Scene3d.Material.color Color.lightPurple)


viewTunnelVertConnectors : Point2d Meters coordinates -> Scene3d.Entity coordinates
viewTunnelVertConnectors point =
    let
        point3d =
            Point3d.on
                SketchPlane3d.xz
                point
    in
    point3d
        |> Point3d.translateIn Direction3d.positiveY (Length.meters 90)
        |> LineSegment3d.from
            (point3d
                |> Point3d.translateIn Direction3d.positiveY (Length.meters -50)
            )
        |> List.singleton
        |> Scene3d.Mesh.lineSegments
        |> Scene3d.mesh
            (Scene3d.Material.color Color.lightPurple)


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
