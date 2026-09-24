module Pages.GlobalCaseManagement.Update exposing (update)

import App.Model
import Backend.AcuteIllnessEncounter.Model exposing (emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model exposing (emptyHIVEncounter)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getAcuteIllnessEncountersForParticipant
        , getHIVEncountersForParticipant
        , getHomeVisitEncountersForParticipant
        , getTuberculosisEncountersForParticipant
        , getWellChildEncountersForParticipant
        )
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model exposing (emptyPrenatalEncounter)
import Backend.TuberculosisEncounter.Model exposing (emptyTuberculosisEncounter)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..), emptyWellChildEncounter)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (EncounterStartedToday(..), FollowUpAcuteIllnessData, FollowUpDialogState(..), FollowUpEncounterDataType(..), FollowUpHIVData, FollowUpNutritionData, FollowUpTuberculosisData, Model, Msg(..))
import Pages.GlobalCaseManagement.Utils exposing (resolveEncounterStartedToday)
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
            ( { model | dialogState = Maybe.map (resolveDialogState currentDate healthCenterId db) state }
            , Cmd.none
            , []
            )

        StartFollowUpEncounter dataType ->
            ( { model | dialogState = Nothing }
            , Cmd.none
            , followUpEncounterMsgs currentDate healthCenterId db dataType
                |> Maybe.withDefault []
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


{-| The messages that act on a follow up entry, or Nothing when an encounter of
that type already took place today and no new one can be started.
-}
followUpEncounterMsgs : NominalDate -> Maybe HealthCenterId -> ModelIndexedDb -> FollowUpEncounterDataType -> Maybe (List App.Model.Msg)
followUpEncounterMsgs currentDate healthCenterId db dataType =
    Maybe.map
        (\selectedHealthCenter ->
            case dataType of
                FollowUpNutrition data ->
                    startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data

                FollowUpAcuteIllness data ->
                    startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data

                FollowUpImmunization data ->
                    startFollowUpEncounterWellChild currentDate selectedHealthCenter db data

                FollowUpTuberculosis data ->
                    startFollowUpEncounterTuberculosis currentDate selectedHealthCenter db data

                FollowUpHIV data ->
                    startFollowUpEncounterHIV currentDate selectedHealthCenter db data

                -- We should never get here, as Prenatal Encounter got it's own action.
                FollowUpPrenatal _ ->
                    Just []

                CaseManagementContactsTracing ->
                    -- We should never get here, as Contacts Tracing got it's own action.
                    Just []
        )
        healthCenterId
        |> Maybe.withDefault (Just [])


{-| A follow up entry that cannot start an encounter today says so, rather than
asking a question it will not act on.
-}
resolveDialogState : NominalDate -> Maybe HealthCenterId -> ModelIndexedDb -> FollowUpDialogState -> FollowUpDialogState
resolveDialogState currentDate healthCenterId db state =
    case state of
        DialogEncounterAlreadyTookPlaceToday _ ->
            state

        DialogStartFollowUpEncounter dataType ->
            case followUpEncounterMsgs currentDate healthCenterId db dataType of
                Just _ ->
                    state

                Nothing ->
                    DialogEncounterAlreadyTookPlaceToday dataType


startFollowUpEncounterHomeVisit : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpNutritionData -> Maybe (List App.Model.Msg)
startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data =
    resolveIndividualParticipantForPerson data.personId HomeVisitEncounter db
        |> Maybe.map
            -- If home visit participant exists, create new encounter for it.
            (\sessionId ->
                unlessEncounterStartedToday currentDate
                    HomeVisitEncounterPage
                    (getHomeVisitEncountersForParticipant db sessionId)
                    [ emptyHomeVisitEncounter sessionId currentDate (Just selectedHealthCenter)
                        |> Backend.Model.PostHomeVisitEncounter
                        |> App.Model.MsgIndexedDb
                    ]
            )
        -- If not, create it.
        |> Maybe.withDefault
            (Just
                [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                    |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                    |> App.Model.MsgIndexedDb
                ]
            )


startFollowUpEncounterAcuteIllness : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpAcuteIllnessData -> Maybe (List App.Model.Msg)
startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data =
    unlessEncounterActiveToday currentDate
        AcuteIllnessEncounterPage
        (getAcuteIllnessEncountersForParticipant db data.participantId
            -- A nurse encounter is not ours to enter, and ours is not theirs.
            |> List.filter (Tuple.second >> .encounterType >> (==) AcuteIllnessEncounterCHW)
        )
        [ emptyAcuteIllnessEncounter data.participantId currentDate data.sequenceNumber AcuteIllnessEncounterCHW (Just selectedHealthCenter)
            |> Backend.Model.PostAcuteIllnessEncounter
            |> App.Model.MsgIndexedDb
        ]


startFollowUpEncounterWellChild : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpNutritionData -> Maybe (List App.Model.Msg)
startFollowUpEncounterWellChild currentDate selectedHealthCenter db data =
    resolveIndividualParticipantForPerson data.personId WellChildEncounter db
        |> Maybe.map
            -- If well child participant exists, create new encounter for it.
            (\sessionId ->
                unlessEncounterStartedToday currentDate
                    WellChildEncounterPage
                    (getWellChildEncountersForParticipant db sessionId
                        -- A nurse encounter is not ours to enter, and does not
                        -- stand in for the CHW visit.
                        |> List.filter (Tuple.second >> .encounterType >> (/=) PediatricCare)
                    )
                    [ emptyWellChildEncounter sessionId currentDate PediatricCareChw (Just selectedHealthCenter)
                        |> Backend.Model.PostWellChildEncounter
                        |> App.Model.MsgIndexedDb
                    ]
            )
        -- We should never get here, since Next Visist follow up is generated from content of
        -- Well Child encounter, which means that participant must exist.
        |> Maybe.withDefault (Just [])


startFollowUpEncounterTuberculosis : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpTuberculosisData -> Maybe (List App.Model.Msg)
startFollowUpEncounterTuberculosis currentDate selectedHealthCenter db data =
    -- If participant was provided, we create new encounter for existing participant.
    Maybe.map
        (\participantId ->
            unlessEncounterStartedToday currentDate
                TuberculosisEncounterPage
                (getTuberculosisEncountersForParticipant db participantId)
                [ emptyTuberculosisEncounter participantId currentDate (Just selectedHealthCenter)
                    |> Backend.Model.PostTuberculosisEncounter
                    |> App.Model.MsgIndexedDb
                ]
        )
        data.participantId
        |> -- Participant was not provided, so we create new participant (which
           -- also creates encounter for newly created participant).
           Maybe.withDefault
            (Just
                [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.TuberculosisEncounter selectedHealthCenter
                    |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                    |> App.Model.MsgIndexedDb
                ]
            )


startFollowUpEncounterHIV : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpHIVData -> Maybe (List App.Model.Msg)
startFollowUpEncounterHIV currentDate selectedHealthCenter db data =
    -- If participant was provided, we create new encounter for existing participant.
    Maybe.map
        (\participantId ->
            unlessEncounterStartedToday currentDate
                HIVEncounterPage
                (getHIVEncountersForParticipant db participantId)
                [ emptyHIVEncounter participantId currentDate (Just selectedHealthCenter)
                    |> Backend.Model.PostHIVEncounter
                    |> App.Model.MsgIndexedDb
                ]
        )
        data.participantId
        |> -- Participant was not provided, so we create new participant (which
           -- also creates encounter for newly created participant).
           Maybe.withDefault
            (Just
                [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HIVEncounter selectedHealthCenter
                    |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                    |> App.Model.MsgIndexedDb
                ]
            )


{-| A follow up entry must never open a second encounter on a day the patient
already had one, which is the rule the participant pages apply. When today's
encounter is still open we go to it, and when it was already completed there is
nothing to do. Otherwise, we run the messages that create a new encounter.
-}
unlessEncounterStartedToday :
    NominalDate
    -> (encounterId -> UserPage)
    -> List ( encounterId, { a | startDate : NominalDate, endDate : Maybe NominalDate } )
    -> List App.Model.Msg
    -> Maybe (List App.Model.Msg)
unlessEncounterStartedToday currentDate encounterPage encounters createMsgs =
    case resolveEncounterStartedToday currentDate encounters of
        EncounterActiveToday encounterId ->
            Just <| navigateToEncounterMsgs encounterPage encounterId

        EncounterCompletedToday ->
            Nothing

        NoEncounterStartedToday ->
            Just createMsgs


{-| Acute Illness follows a different rule, which its participant page applies:
a patient whose condition changes is seen again the same day, on a subsequent
encounter. So only an encounter still open takes the place of a new one.
-}
unlessEncounterActiveToday :
    NominalDate
    -> (encounterId -> UserPage)
    -> List ( encounterId, { a | startDate : NominalDate, endDate : Maybe NominalDate } )
    -> List App.Model.Msg
    -> Maybe (List App.Model.Msg)
unlessEncounterActiveToday currentDate encounterPage encounters createMsgs =
    case resolveEncounterStartedToday currentDate encounters of
        EncounterActiveToday encounterId ->
            Just <| navigateToEncounterMsgs encounterPage encounterId

        EncounterCompletedToday ->
            Just createMsgs

        NoEncounterStartedToday ->
            Just createMsgs


navigateToEncounterMsgs : (encounterId -> UserPage) -> encounterId -> List App.Model.Msg
navigateToEncounterMsgs encounterPage encounterId =
    [ App.Model.SetActivePage <| UserPage <| encounterPage encounterId ]
