module Pages.AcuteIllness.Outcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.IndividualEncounterParticipant.Utils exposing (acuteIllnessOutcomeFromString)
import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Outcome.Model exposing (..)
import Pages.Page exposing (Page(..))


update : NominalDate -> IndividualEncounterParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SaveAcuteIllnessOutcome ->
            model.acuteIllnessOutcome
                |> Maybe.map
                    (\outcome ->
                        ( model
                        , Cmd.none
                        , [ Backend.IndividualEncounterParticipant.Model.CloseAcuteIllnessSession outcome
                                |> Backend.Model.MsgIndividualEncounterParticipant id
                                |> App.Model.MsgIndexedDb
                          , App.Model.SetActivePage PinCodePage
                          ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none, [] )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAcuteIllnessOutcome value ->
            let
                outcome =
                    acuteIllnessOutcomeFromString value
            in
            ( { model | acuteIllnessOutcome = outcome }
            , Cmd.none
            , []
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )
