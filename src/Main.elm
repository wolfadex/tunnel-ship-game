module Main exposing
    ( Flags
    , Model
    , main
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Events
import Camera3d
import Color
import CubicSpline3d exposing (CubicSpline3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Html
import Html.Attributes
import Json.Decode
import Length exposing (Length, Meters)
import LineSegment3d
import Pixels
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d
import Quantity exposing (Quantity)
import Race
import Random
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set exposing (Set)
import Shape exposing (Shape)
import SketchPlane3d exposing (SketchPlane3d)
import Speed exposing (Speed)
import Time
import Update exposing (Update)
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


type Model
    = Editor
    | Racing Race.Model


type alias Flags =
    Float


init : Flags -> Update Model Msg
init timeNow =
    Race.init timeNow
        |> Update.mapModel Racing
        |> Update.mapMsg RaceMessage


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Editor ->
            Sub.none

        Racing raceModel ->
            Race.subscriptions raceModel
                |> Sub.map RaceMessage


type Msg
    = RaceMessage Race.Msg


update : Msg -> Model -> Update Model Msg
update msg model =
    case model of
        Editor ->
            model
                |> Update.save

        Racing raceModel ->
            case msg of
                RaceMessage raceMsg ->
                    Race.update raceMsg raceModel
                        |> Update.mapModel Racing
                        |> Update.mapMsg RaceMessage


view : Model -> Browser.Document Msg
view model =
    { title = "Tunnle Rocket"
    , body =
        case model of
            Editor ->
                viewEditor

            Racing raceModel ->
                Race.view raceModel
                    |> List.map (Html.map RaceMessage)
    }


viewEditor : List (Html.Html msg)
viewEditor =
    []
