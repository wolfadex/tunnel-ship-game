module Main exposing
    ( Flags
    , Model
    , main
    )

import Browser
import Browser.Events
import Html
import Html.Attributes
import Race
import Random
import Set exposing (Set)
import Time
import TrackEditor
import Update exposing (Update)


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
    = TrackEditor TrackEditor.Model
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
        TrackEditor trackModel ->
            TrackEditor.subscriptions trackModel
                |> Sub.map TrackEditorMessage

        Racing raceModel ->
            Race.subscriptions raceModel
                |> Sub.map RaceMessage


type Msg
    = RaceMessage Race.Msg
    | TrackEditorMessage TrackEditor.Msg


update : Msg -> Model -> Update Model Msg
update msg model =
    case model of
        TrackEditor trackModel ->
            case msg of
                TrackEditorMessage trackMsg ->
                    TrackEditor.update trackMsg trackModel
                        |> Update.mapModel TrackEditor
                        |> Update.mapMsg TrackEditorMessage

                _ ->
                    model
                        |> Update.save

        Racing raceModel ->
            case msg of
                RaceMessage raceMsg ->
                    Race.update raceMsg raceModel
                        |> Update.mapModel Racing
                        |> Update.mapMsg RaceMessage

                _ ->
                    model
                        |> Update.save


view : Model -> Browser.Document Msg
view model =
    { title = "Tunnle Rocket"
    , body =
        case model of
            TrackEditor trackModel ->
                TrackEditor.view trackModel
                    |> List.map (Html.map TrackEditorMessage)

            Racing raceModel ->
                Race.view raceModel
                    |> List.map (Html.map RaceMessage)
    }
