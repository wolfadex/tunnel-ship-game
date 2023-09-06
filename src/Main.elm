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
import Angle
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
import Quantity
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
import Update exposing (Update)
import Util.Debug
import Util.Function
import Util.List
import Util.Maybe
import Util.Random
import Vector3d exposing (Vector3d)
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

    -- , otherShips : List Ship
    , seed : Random.Seed
    , keysDown : Set String
    , track : Track
    }


type alias Track =
    { shape : Shape WorldCoordinates
    , path : CubicSpline3d Meters WorldCoordinates
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
    }


type alias RotatingDetails =
    { travelTime : Float
    , direction : Direction
    , shootOnComplete : Bool
    }


type WorldCoordinates
    = WorldCoordinates Never


initShip : CubicSpline3d Meters WorldCoordinates -> Shape WorldCoordinates -> Ship
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
    }


init : () -> Update Model Msg
init () =
    let
        initialShape =
            -- Shape.custom
            Shape.newRegular 24

        path =
            CubicSpline3d.fromControlPoints
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 0)
                    (Length.meters 0)
                )
                (Point3d.xyz
                    (Length.meters 10)
                    (Length.meters 33)
                    (Length.meters 0)
                )
                (Point3d.xyz
                    (Length.meters -10)
                    (Length.meters 66)
                    (Length.meters -10)
                )
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 100)
                    (Length.meters 0)
                )
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
    -- , lasers = []
    -- , enemies = []
    -- , nextEnemySpawn = enemySpawnRate
    , seed = Random.initialSeed 0

    -- , shape = initialShape
    -- , tunnelOffset = 0
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
                                    p =
                                        CubicSpline3d.pointOn path (toFloat i / segments)

                                    dir =
                                        CubicSpline3d.firstDerivative path (toFloat i / segments)
                                            |> Vector3d.direction
                                            |> Maybe.withDefault Direction3d.positiveY
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
                                    p =
                                        CubicSpline3d.pointOn path (toFloat i / segments)

                                    dir =
                                        CubicSpline3d.secondDerivative path (toFloat i / segments)
                                            |> Vector3d.direction
                                            |> Maybe.withDefault Direction3d.positiveY
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
    }
        |> Update.save


sketchPlaneAt : CubicSpline3d Meters coordinates -> Float -> SketchPlane3d Meters coordinates defines
sketchPlaneAt path dist =
    let
        center =
            CubicSpline3d.pointOn path dist

        normal =
            CubicSpline3d.firstDerivative path dist
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveY

        upDir =
            CubicSpline3d.secondDerivative path dist
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveY

        xDir =
            Vector3d.cross
                (Direction3d.toVector normal)
                (Direction3d.toVector upDir)
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveX
    in
    SketchPlane3d.unsafe
        { originPoint = center
        , xDirection = xDir
        , yDirection = upDir
        }


viewTunnelRing : Shape coordinates -> CubicSpline3d Meters coordinates -> Float -> Scene3d.Entity coordinates
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
        [ Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
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
    = Tick Duration
    | KeyDown String
    | KeyUp String
      -- | Shoot
    | Move Direction


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
                -- |> rotateShip deltaMs
                |> moveShip deltaMs

        -- |> moveLasers deltaMs
        -- |> spawnEnemy deltaMs
        -- |> killEnemies
        -- |> moveEnemies deltaMs
        -- |> moveTunnel deltaMs
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
            -- { model
            --     | ship =
            --         { ship
            --             | rotating =
            --                 case ship.rotating of
            --                     Nothing ->
            --                         Just
            --                             { travelTime = rotateAnimationTime
            --                             , direction = direction
            --                             , shootOnComplete = False
            --                             }
            --                     Just _ ->
            --                         ship.rotating
            --         }
            -- }
            model
                |> Update.save



-- Shoot ->
--     case model.ship.rotating of
--         Nothing ->
--             model
--                 |> shootLaser
--                 |> Update.save
--         Just rotating ->
--             let
--                 ship =
--                     model.ship
--             in
--             { model
--                 | ship =
--                     { ship
--                         | rotating =
--                             Just { rotating | shootOnComplete = True }
--                     }
--             }
--                 |> Update.save


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
                -- |> applyKey model " " (Update.withMsg Shoot)
                |> applyKey model "ArrowLeft" (Update.withMsg (Move Clockwise))
                |> applyKey model "ArrowRight" (Update.withMsg (Move CounterClockwise))
        )


tunnelMoveMax =
    10


enemySpawnRate =
    2000



-- spawnEnemy : Float -> Update Model Msg -> Update Model Msg
-- spawnEnemy deltaMs =
--     Update.mapModel
--         (\model ->
--             let
--                 nextEnemySpawn =
--                     model.nextEnemySpawn - deltaMs
--             in
--             if nextEnemySpawn <= 0 then
--                 let
--                     ( spawnPoint, seed ) =
--                         Random.step
--                             (model.shape
--                                 |> Polygon2d.edges
--                                 |> List.map
--                                     (LineSegment3d.on
--                                         (SketchPlane3d.xz
--                                             |> SketchPlane3d.translateIn Direction3d.positiveY (Length.meters 30)
--                                         )
--                                         >> LineSegment3d.midpoint
--                                     )
--                                 |> Util.Random.fromList
--                                 |> Random.map (Maybe.withDefault Point3d.origin)
--                             )
--                             model.seed
--                 in
--                 { model
--                     | nextEnemySpawn = nextEnemySpawn + enemySpawnRate
--                     , enemies =
--                         { location =
--                             spawnPoint
--                                 |> Point3d.translateIn Direction3d.positiveY (Length.meters 30)
--                         }
--                             :: model.enemies
--                     , seed = seed
--                 }
--             else
--                 { model
--                     | nextEnemySpawn = nextEnemySpawn
--                 }
--         )
-- moveEnemies : Float -> Update Model Msg -> Update Model Msg
-- moveEnemies deltaMs =
--     Update.mapModel
--         (\model ->
--             { model
--                 | enemies =
--                     List.filterMap (moveEnemy deltaMs) model.enemies
--             }
--         )


enemyMoveSpeed =
    50



-- moveEnemy : Float -> Enemy -> Maybe Enemy
-- moveEnemy deltaMs enemy =
--     let
--         newLocation =
--             enemy.location
--                 |> Point3d.translateIn Direction3d.negativeY (Length.meters (deltaMs / enemyMoveSpeed))
--     in
--     if Point3d.yCoordinate newLocation |> Quantity.lessThan (Length.meters -10) then
--         Nothing
--     else
--         Just
--             { enemy
--                 | location = newLocation
--             }
-- killEnemies : Update Model Msg -> Update Model Msg
-- killEnemies =
--     Update.mapModel
--         (\model ->
--             let
--                 remaining =
--                     checkLaserCollisions { enemies = model.enemies, lasers = model.lasers }
--             in
--             { model
--                 | enemies = remaining.enemies
--                 , lasers = remaining.lasers
--                 -- TODO: Add explosion animation
--                 -- , enemiesToExplode = remaining.destroyedEnemies
--             }
--         )


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



-- shootLaser : Model -> Model
-- shootLaser model =
--     { model
--         | lasers =
--             Block3d.centerPoint model.ship.body :: model.lasers
--     }


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
            in
            { model
                | ship =
                    { ship
                        | speed =
                            ship.acceleration
                                |> Quantity.for deltaTime
                                |> Quantity.plus ship.speed
                                |> Quantity.min (Speed.kilometersPerHour 1000)
                        , distance =
                            if distance |> Quantity.greaterThan (Length.kilometers 10) then
                                Length.kilometers 0

                            else
                                distance
                    }
            }
        )



-- rotateShip : Duration -> Update Model Msg -> Update Model Msg
-- rotateShip deltaMs =
--     Update.mapModel
--         (\model ->
--             case model.ship.rotating of
--                 Nothing ->
--                     model
--                 Just ({ travelTime, direction } as rotating) ->
--                     let
--                         ship : Ship
--                         ship =
--                             model.ship
--                         remainingTime : Float
--                         remainingTime =
--                             travelTime - Duration.inMilliseconds deltaMs
--                         angle =
--                             let
--                                 point1 : Maybe (Point2d Meters WorldCoordinates)
--                                 point1 =
--                                     model.shape
--                                         |> Polygon2d.vertices
--                                         |> List.Extra.getAt ship.rocket1Index
--                                 point2 : Maybe (Point2d Meters WorldCoordinates)
--                                 point2 =
--                                     model.shape
--                                         |> Polygon2d.vertices
--                                         |> List.Extra.getAt ship.rocket2Index
--                                 point3 : Maybe (Point2d Meters WorldCoordinates)
--                                 point3 =
--                                     model.shape
--                                         |> Polygon2d.vertices
--                                         |> List.Extra.getAt
--                                             (modBy (Shape.sideCount model.shape) <|
--                                                 case direction of
--                                                     Clockwise ->
--                                                         if ship.upIsNormal then
--                                                             ship.rocket1Index - 1
--                                                         else
--                                                             ship.rocket2Index - 1
--                                                     CounterClockwise ->
--                                                         if ship.upIsNormal then
--                                                             ship.rocket2Index + 1
--                                                         else
--                                                             ship.rocket1Index + 1
--                                             )
--                             in
--                             case direction of
--                                 Clockwise ->
--                                     if ship.upIsNormal then
--                                         Maybe.map2 Direction2d.angleFrom
--                                             (Util.Maybe.andThen2 Direction2d.from point1 point2)
--                                             (Util.Maybe.andThen2 Direction2d.from point1 point3)
--                                             |> Maybe.map Quantity.negate
--                                     else
--                                         Maybe.map2 Direction2d.angleFrom
--                                             (Util.Maybe.andThen2 Direction2d.from point2 point1)
--                                             (Util.Maybe.andThen2 Direction2d.from point2 point3)
--                                             |> Maybe.map Quantity.negate
--                                 CounterClockwise ->
--                                     if ship.upIsNormal then
--                                         Maybe.map2 Direction2d.angleFrom
--                                             (Util.Maybe.andThen2 Direction2d.from point2 point1)
--                                             (Util.Maybe.andThen2 Direction2d.from point2 point3)
--                                             |> Maybe.map Quantity.negate
--                                     else
--                                         Maybe.map2 Direction2d.angleFrom
--                                             (Util.Maybe.andThen2 Direction2d.from point1 point3)
--                                             (Util.Maybe.andThen2 Direction2d.from point1 point2)
--                         distanceToRotate =
--                             angle
--                                 |> Maybe.map (Quantity.divideBy rotateAnimationTime)
--                                 |> Maybe.withDefault (Angle.degrees 0)
--                                 |> (if remainingTime > 0 then
--                                         Quantity.multiplyBy (Duration.inMilliseconds deltaMs)
--                                     else
--                                         Quantity.multiplyBy travelTime
--                                    )
--                         newRocket1 =
--                             if (ship.upIsNormal && direction == CounterClockwise) || (not ship.upIsNormal && direction == Clockwise) then
--                                 Cylinder3d.rotateAround
--                                     (Cylinder3d.axis ship.rocket2)
--                                     distanceToRotate
--                                     ship.rocket1
--                             else
--                                 ship.rocket1
--                         newRocket2 =
--                             if (ship.upIsNormal && direction == Clockwise) || (not ship.upIsNormal && direction == CounterClockwise) then
--                                 Cylinder3d.rotateAround
--                                     (Cylinder3d.axis ship.rocket1)
--                                     distanceToRotate
--                                     ship.rocket2
--                             else
--                                 ship.rocket2
--                         newShip =
--                             { ship
--                                 | rocket1 = newRocket1
--                                 , rocket2 = newRocket2
--                                 , body =
--                                     let
--                                         bodyOffsetInterval =
--                                             Interval.from -0.125 0.125
--                                         a : Point3d Meters WorldCoordinates
--                                         a =
--                                             newRocket1
--                                                 |> Cylinder3d.centerPoint
--                                         b : Point3d Meters WorldCoordinates
--                                         b =
--                                             newRocket2
--                                                 |> Cylinder3d.centerPoint
--                                         frame3d =
--                                             LineSegment3d.from a b
--                                                 |> LineSegment3d.midpoint
--                                                 |> Frame3d.withXDirection
--                                                     (Direction3d.from a b
--                                                         |> Maybe.withDefault Direction3d.positiveX
--                                                     )
--                                     in
--                                     Block3d.centeredOn frame3d
--                                         ( Length.meters 0.25
--                                         , Length.meters 0.125
--                                         , Length.meters 1
--                                         )
--                                         |> Block3d.translateIn
--                                             (Frame3d.yDirection frame3d)
--                                             (max 0 remainingTime
--                                                 |> (\x ->
--                                                         if ship.upIsNormal then
--                                                             rotateAnimationTime - x
--                                                         else
--                                                             x
--                                                    )
--                                                 |> normalize 0 rotateAnimationTime
--                                                 |> Interval.interpolate bodyOffsetInterval
--                                                 |> Length.meters
--                                             )
--                             }
--                                 |> shipDoneRotating model.shape rotating direction remainingTime
--                     in
--                     { model | ship = newShip }
--          -- |> Util.Function.applyIf (rotating.shootOnComplete && remainingTime <= 0) shootLaser
--         )
-- shipDoneRotating : Shape WorldCoordinates -> RotatingDetails -> Direction -> Float -> Ship -> Ship
-- shipDoneRotating shape rotating direction remainingTime ship =
--     if remainingTime > 0 then
--         { ship
--             | rotating =
--                 Just { rotating | travelTime = remainingTime }
--         }
--     else
--         { ship
--             | rotating = Nothing
--             , upIsNormal = not ship.upIsNormal
--             , rocket1Index =
--                 case direction of
--                     Clockwise ->
--                         if ship.upIsNormal then
--                             ship.rocket1Index
--                         else
--                             (ship.rocket2Index - 1)
--                                 |> modBy (Shape.sideCount shape)
--                     CounterClockwise ->
--                         if ship.upIsNormal then
--                             (ship.rocket2Index + 1)
--                                 |> modBy (Shape.sideCount shape)
--                         else
--                             ship.rocket1Index
--             , rocket2Index =
--                 case direction of
--                     Clockwise ->
--                         if ship.upIsNormal then
--                             (ship.rocket1Index - 1)
--                                 |> modBy (Shape.sideCount shape)
--                         else
--                             ship.rocket2Index
--                     CounterClockwise ->
--                         if ship.upIsNormal then
--                             ship.rocket2Index
--                         else
--                             (ship.rocket1Index + 1)
--                                 |> modBy (Shape.sideCount shape)
--         }
-- moveLasers : Float -> Update Model Msg -> Update Model Msg
-- moveLasers deltaMs =
--     Update.mapModel
--         (\model ->
--             { model
--                 | lasers = List.filterMap (moveLaser deltaMs) model.lasers
--             }
--         )
-- moveLaser : Float -> Point3d Meters WorldCoordinates -> Maybe (Point3d Meters WorldCoordinates)
-- moveLaser deltaMs point =
--     let
--         newPoint =
--             Point3d.translateIn Direction3d.positiveY (Length.meters (deltaMs * 0.1)) point
--     in
--     if Point3d.yCoordinate newPoint |> Quantity.greaterThan (Length.meters 70) then
--         Nothing
--     else
--         Just newPoint


rotateAnimationTime =
    150


normalize : Float -> Float -> Float -> Float
normalize minValue maxValue x =
    (x - minValue) / (maxValue - minValue)


view : Model -> Browser.Document Msg
view model =
    { title = "Tunnle Rocket"
    , body =
        let
            eyeDist =
                model.ship.distance
                    |> Length.inKilometers
                    |> (\d -> d - 1)
                    |> normalize 0 10

            shipDist =
                model.ship.distance
                    |> Length.inKilometers
                    |> normalize 0 10

            focalPoint =
                shipDist
                    |> CubicSpline3d.pointOn model.track.path

            zDir =
                shipDist
                    |> CubicSpline3d.secondDerivative model.track.path
                    |> Vector3d.direction
                    |> Maybe.withDefault Direction3d.positiveZ

            yDir =
                shipDist
                    |> CubicSpline3d.firstDerivative model.track.path
                    |> Vector3d.direction
                    |> Maybe.withDefault Direction3d.positiveZ

            xDir =
                Vector3d.cross
                    (Direction3d.toVector zDir)
                    (Direction3d.toVector yDir)
                    |> Vector3d.direction
                    |> Maybe.withDefault Direction3d.positiveX
        in
        [ Scene3d.cloudy
            { dimensions = ( Pixels.int 800, Pixels.int 600 )
            , upDirection = Direction3d.negativeY
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { eyePoint =
                                eyeDist
                                    |> CubicSpline3d.pointOn model.track.path
                                    |> Point3d.translateIn (Direction3d.reverse zDir) (Length.meters 0.5)
                                    |> Point3d.translateIn (Direction3d.reverse xDir) (Length.meters 0.5)
                            , focalPoint = focalPoint
                            , upDirection = Direction3d.reverse zDir
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


viewShip : Point3d Meters WorldCoordinates -> CubicSpline3d Meters WorldCoordinates -> Ship -> Scene3d.Entity WorldCoordinates
viewShip focalPoint path ship =
    let
        dist =
            ship.distance
                |> Length.inKilometers
                |> normalize 0 10

        zDir =
            dist
                |> CubicSpline3d.secondDerivative path
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveZ

        yDir =
            dist
                |> CubicSpline3d.firstDerivative path
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveZ

        xDir =
            Vector3d.cross
                (Direction3d.toVector zDir)
                (Direction3d.toVector yDir)
                |> Vector3d.direction
                |> Maybe.withDefault Direction3d.positiveX
    in
    ship.geometry
        |> Block3d.placeIn
            (sketchPlaneAt path dist
                |> SketchPlane3d.toFrame
            )
        |> Block3d.translateIn zDir (Length.meters 0.75)
        |> Scene3d.block (Scene3d.Material.matte Color.green)


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
