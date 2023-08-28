module Main exposing (..)

import Angle
import Block3d exposing (Block3d)
import Browser
import Browser.Events
import Camera3d
import Color
import Cylinder3d exposing (Cylinder3d)
import Direction2d
import Direction3d
import Frame2d
import Frame3d
import Html exposing (Html)
import Interval
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment2d
import LineSegment3d
import Pixels
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Quantity
import Rectangle2d
import Rectangle3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import SketchPlane3d
import Util.Function
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
    }


type alias Laser =
    Point3d Meters WorldCoordinates


type alias Ship =
    { rocket1 : Cylinder3d Meters WorldCoordinates
    , rocket2 : Cylinder3d Meters WorldCoordinates
    , body : Block3d Meters WorldCoordinates
    , moving :
        Maybe
            { travelTime : Float
            , direction : Direction
            , shootOnComplete : Bool
            }
    , upIsNormal : Bool
    }


type WorldCoordinates
    = WorldCoordinates Never


initialShip : Ship
initialShip =
    octagon
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
                , rocket2 = makeRailRocket b
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
    ( { ship = initialShip
      , lasers = []
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
                ship =
                    model.ship

                remainingTime =
                    travelTime - deltaMs

                travelDistancePerMs =
                    135 / rotateAnimationTime

                distanceToRotate =
                    travelDistancePerMs
                        * (if remainingTime > 0 then
                            deltaMs

                           else
                            travelTime
                          )

                newRocket1 =
                    if (ship.upIsNormal && direction == CounterClockwise) || (not ship.upIsNormal && direction == Clockwise) then
                        Cylinder3d.rotateAround
                            (Cylinder3d.axis ship.rocket2)
                            (Angle.degrees <|
                                if direction == CounterClockwise then
                                    distanceToRotate

                                else
                                    negate distanceToRotate
                            )
                            ship.rocket1

                    else
                        ship.rocket1

                newRocket2 =
                    if (ship.upIsNormal && direction == Clockwise) || (not ship.upIsNormal && direction == CounterClockwise) then
                        Cylinder3d.rotateAround
                            (Cylinder3d.axis ship.rocket1)
                            (Angle.degrees <|
                                if direction == Clockwise then
                                    negate distanceToRotate

                                else
                                    distanceToRotate
                            )
                            ship.rocket2

                    else
                        ship.rocket2

                bodyOffsetInterval =
                    Interval.from -0.125 0.125

                newShip =
                    { ship
                        | moving =
                            if remainingTime > 0 then
                                Just { moving | travelTime = remainingTime }

                            else
                                Nothing
                        , rocket1 = newRocket1
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
                        , upIsNormal =
                            if remainingTime > 0 then
                                ship.upIsNormal

                            else
                                not ship.upIsNormal
                    }
            in
            { model | ship = newShip }
                |> Util.Function.applyIf (moving.shootOnComplete && remainingTime <= 0) shootLaser


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
    if Point3d.yCoordinate newPoint |> Quantity.greaterThan (Length.meters 50) then
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
        [ Html.text "Hello, World!"
        , Scene3d.cloudy
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
                [ tunnelRing (Length.meters 0)
                , tunnelRing (Length.meters 10)
                , tunnelRing (Length.meters 20)
                , tunnelRing (Length.meters 30)
                , [ makeRailThingy model.ship.rocket1
                  , makeRailThingy model.ship.rocket2
                  , Scene3d.block (Scene3d.Material.matte Color.green)
                        model.ship.body
                  ]
                    |> Scene3d.group
                , model.lasers
                    |> List.map viewLaser
                    |> Scene3d.group
                ]
            }
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


octagon : Polygon2d Meters coordinates
octagon =
    Polygon2d.regular
        { centerPoint = Point2d.origin
        , circumradius = Length.meters 1
        , numSides = 8
        }


tunnelRing : Length -> Scene3d.Entity coordinates
tunnelRing distance =
    octagon
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
