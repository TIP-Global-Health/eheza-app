module Pages.AcuteIllnessOutcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Decoder exposing (acuteIllnessOutcomeFromString, pregnancyOutcomeFromString)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllnessOutcome.Model exposing (..)
import Pages.Page exposing (Page(..))


update : NominalDate -> IndividualEncounterParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        NoOp ->
            noChange

        SaveAcuteIllnessOutcome ->
            model.acuteIllnessOutcome
                |> Maybe.map
                    (\outcome ->
                        ( model
                        , Cmd.none
                        , [ Backend.IndividualEncounterParticipant.Model.CloseAcuteIllnessSession outcome
                                |> Backend.Model.MsgIndividualSession id
                                |> App.Model.MsgIndexedDb
                          , App.Model.SetActivePage PinCodePage
                          ]
                        )
                    )
                |> Maybe.withDefault noChange

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
