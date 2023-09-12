module Main exposing
    ( Flags
    , Model
    , Msg
    , main
    )

import Browser
import Html
import Race
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


init : Flags -> Update Model Msg Effect
init timeNow =
    TrackEditor.init timeNow
        |> Update.mapModel TrackEditor
        |> Update.mapMsg TrackEditorMessage
        |> Update.applyEffects applyTrackEditorEffects


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


type Effect
    = TrackEditorEffect TrackEditor.Effect


update : Msg -> Model -> Update Model Msg Effect
update msg model =
    case model of
        TrackEditor trackModel ->
            case msg of
                TrackEditorMessage trackMsg ->
                    TrackEditor.update trackMsg trackModel
                        |> Update.mapModel TrackEditor
                        |> Update.mapMsg TrackEditorMessage
                        |> Update.applyEffects applyTrackEditorEffects

                _ ->
                    model
                        |> Update.save

        Racing raceModel ->
            case msg of
                RaceMessage raceMsg ->
                    Race.update raceMsg raceModel
                        |> Update.mapModel Racing
                        |> Update.mapMsg RaceMessage
                        |> Update.applyEffects applyRaceEffects

                _ ->
                    model
                        |> Update.save


applyTrackEditorEffects : TrackEditor.Effect -> Update Model Msg Effect -> Update Model Msg Effect
applyTrackEditorEffects effect =
    case effect of
        TrackEditor.TestTrack track ->
            Update.andThen
                (\_ ->
                    Race.init track
                        |> Update.mapModel Racing
                        |> Update.mapMsg RaceMessage
                        |> Update.applyEffects applyRaceEffects
                )


applyRaceEffects : Race.Effect -> Update Model Msg Effect -> Update Model Msg Effect
applyRaceEffects _ =
    identity


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
