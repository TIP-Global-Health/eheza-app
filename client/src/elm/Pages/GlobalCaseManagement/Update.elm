module Pages.GlobalCaseManagement.Update exposing (update)

import App.Model
import Backend.AcuteIllnessEncounter.Model exposing (emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model exposing (emptyHIVEncounter)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model exposing (emptyPrenatalEncounter)
import Backend.TuberculosisEncounter.Model exposing (emptyTuberculosisEncounter)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..), emptyWellChildEncounter)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (FollowUpAcuteIllnessData, FollowUpEncounterDataType(..), FollowUpHIVData, FollowUpNutritionData, FollowUpTuberculosisData, Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Types exposing (WarningPopupType(..))
import Pages.Prenatal.Encounter.Utils exposing (generatePostCreateDestination)
import Pages.Prenatal.RecurrentActivity.Model


update : NominalDate -> Maybe HealthCenterId -> Msg -> ModelIndexedDb -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate healthCenterId msg db model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , []
            )

        SetDialogState state ->
            ( { model | dialogState = state }
            , Cmd.none
            , []
            )

        StartFollowUpEncounter dataType ->
            let
                msgs =
                    healthCenterId
                        |> Maybe.map
                            (\selectedHealthCenter ->
                                case dataType of
                                    FollowUpNutrition data ->
                                        startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data

                                    FollowUpAcuteIllness data ->
                                        startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data

                                    FollowUpImmunization data ->
                                        startFollowUpEncounterWellChild currentDate selectedHealthCenter db data

                                    FollowUpTuberculosis data ->
                                        startFollowUpEncounterTuberculosis currentDate selectedHealthCenter data

                                    FollowUpHIV data ->
                                        startFollowUpEncounterHIV currentDate selectedHealthCenter data

                                    -- We should never get here, as Prenatal Encounter got it's own action.
                                    FollowUpPrenatal _ ->
                                        []

                                    CaseManagementContactsTracing ->
                                        -- We should never get here, as Contacts Tracing got it's own action.
                                        []
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = Nothing }
            , Cmd.none
            , msgs
            )

        StartPrenatalFollowUpEncounter participantId hasNurseEncounter newEncounterType ->
            let
                msgs =
                    healthCenterId
                        |> Maybe.map
                            (\selectedHealthCenter ->
                                [ emptyPrenatalEncounter participantId currentDate newEncounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostPrenatalEncounter (generatePostCreateDestination newEncounterType hasNurseEncounter)
                                    |> App.Model.MsgIndexedDb
                                ]
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = Nothing }
            , Cmd.none
            , msgs
            )

        HandleUrgentPrenatalDiagnoses encounterId dialogState ->
            ( model
            , Cmd.none
            , -- Navigate to next steps activity and view warning dialog.
              [ PrenatalRecurrentActivityPage encounterId Backend.PrenatalActivity.Model.RecurrentNextSteps
                    |> UserPage
                    |> App.Model.SetActivePage
              , Pages.Prenatal.RecurrentActivity.Model.SetWarningPopupState (Just <| WarningPopupUrgent dialogState)
                    |> App.Model.MsgPagePrenatalRecurrentActivity encounterId Backend.PrenatalActivity.Model.RecurrentNextSteps
                    |> App.Model.MsgLoggedIn
              ]
            )


startFollowUpEncounterHomeVisit : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpNutritionData -> List App.Model.Msg
startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data =
    resolveIndividualParticipantForPerson data.personId HomeVisitEncounter db
        |> Maybe.map
            -- If home visit participant exists, create new encounter for it.
            (\sessionId ->
                [ emptyHomeVisitEncounter sessionId currentDate (Just selectedHealthCenter)
                    |> Backend.Model.PostHomeVisitEncounter
                    |> App.Model.MsgIndexedDb
                ]
            )
        -- If not, create it.
        |> Maybe.withDefault
            [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                |> App.Model.MsgIndexedDb
            ]


startFollowUpEncounterAcuteIllness : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpAcuteIllnessData -> List App.Model.Msg
startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data =
    [ emptyAcuteIllnessEncounter data.participantId currentDate data.sequenceNumber AcuteIllnessEncounterCHW (Just selectedHealthCenter)
        |> Backend.Model.PostAcuteIllnessEncounter
        |> App.Model.MsgIndexedDb
    ]


startFollowUpEncounterWellChild : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpNutritionData -> List App.Model.Msg
startFollowUpEncounterWellChild currentDate selectedHealthCenter db data =
    resolveIndividualParticipantForPerson data.personId WellChildEncounter db
        |> Maybe.map
            -- If well child participant exists, create new encounter for it.
            (\sessionId ->
                [ emptyWellChildEncounter sessionId currentDate PediatricCareChw (Just selectedHealthCenter)
                    |> Backend.Model.PostWellChildEncounter
                    |> App.Model.MsgIndexedDb
                ]
            )
        -- We should never get here, since Next Visist follow up is generated from content of
        -- Well Child encounter, which means that participant must exist.
        |> Maybe.withDefault []


startFollowUpEncounterTuberculosis : NominalDate -> HealthCenterId -> FollowUpTuberculosisData -> List App.Model.Msg
startFollowUpEncounterTuberculosis currentDate selectedHealthCenter data =
    -- If participant was provided, we create new encounter for existing participant.
    Maybe.map
        (\participantId ->
            [ emptyTuberculosisEncounter participantId currentDate (Just selectedHealthCenter)
                |> Backend.Model.PostTuberculosisEncounter
                |> App.Model.MsgIndexedDb
            ]
        )
        data.participantId
        |> -- Participant was not provided, so we create new participant (which
           -- also creates encounter for newly created participant).
           Maybe.withDefault
            [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.TuberculosisEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                |> App.Model.MsgIndexedDb
            ]


startFollowUpEncounterHIV : NominalDate -> HealthCenterId -> FollowUpHIVData -> List App.Model.Msg
startFollowUpEncounterHIV currentDate selectedHealthCenter data =
    -- If participant was provided, we create new encounter for existing participant.
    Maybe.map
        (\participantId ->
            [ emptyHIVEncounter participantId currentDate (Just selectedHealthCenter)
                |> Backend.Model.PostHIVEncounter
                |> App.Model.MsgIndexedDb
            ]
        )
        data.participantId
        |> -- Participant was not provided, so we create new participant (which
           -- also creates encounter for newly created participant).
           Maybe.withDefault
            [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HIVEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                |> App.Model.MsgIndexedDb
            ]
