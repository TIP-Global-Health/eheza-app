module Pages.WellChild.Encounter.Update exposing (update)

import App.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (EncounterNote(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChild.Activity.Model
import Pages.WellChild.Activity.Utils
import Pages.WellChild.Encounter.Model exposing (..)
import Pages.WellChild.Encounter.Utils
import RemoteData
import SyncManager.Model exposing (Site)
import ZScore.Model


update : NominalDate -> ZScore.Model.Model -> Site -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate zscores site isChw db msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.WellChildEncounter.Model.CloseWellChildEncounter
                    |> Backend.Model.MsgWellChildEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
                |> sequenceExtra (update currentDate zscores site isChw db) [ SetDialogState Nothing ]

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        TriggerAcuteIllnessEncounter childId encounterId ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (AcuteIllnessParticipantPage InitiatorParticipantsPage childId))
                    , SetDialogState Nothing
                    ]

                markEncounterAsAITriggerMsg =
                    [ Backend.WellChildEncounter.Model.SetWellChildEncounterNote NoteTriggeredAcuteIllnessEncounter
                        |> Backend.Model.MsgWellChildEncounter encounterId
                        |> App.Model.MsgIndexedDb
                    ]
            in
            ( model
            , Cmd.none
            , markEncounterAsAITriggerMsg
            )
                |> sequenceExtra (update currentDate zscores site isChw db) extraMsgs

        NavigateToActivity encounterId activity ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (WellChildActivityPage encounterId activity)) ]

                -- View Assessment popup when navigating to Next Steps activity.
                appMsgs =
                    if activity == WellChildNextSteps then
                        Pages.WellChild.Encounter.Utils.generateAssembledData site encounterId db
                            |> RemoteData.toMaybe
                            |> Maybe.map
                                (\assembled ->
                                    let
                                        mandatoryActivitiesCompleted =
                                            Pages.WellChild.Activity.Utils.mandatoryNutritionAssessmentTasksCompleted
                                                currentDate
                                                assembled
                                    in
                                    if not mandatoryActivitiesCompleted then
                                        -- Assement is done only when all mandatory measurements were recorded.
                                        []

                                    else
                                        let
                                            assessment =
                                                Pages.WellChild.Activity.Utils.generateNutritionAssessment currentDate zscores db assembled
                                        in
                                        if List.isEmpty assessment then
                                            []

                                        else
                                            -- Show warning popup with new assesment.
                                            Pages.WellChild.Activity.Model.PopupNutritionAssessment assessment
                                                |> Just
                                                |> Pages.WellChild.Activity.Model.SetWarningPopupState
                                                |> App.Model.MsgPageWellChildActivity encounterId Backend.WellChildActivity.Model.WellChildNextSteps
                                                |> App.Model.MsgLoggedIn
                                                |> List.singleton
                                )
                            |> Maybe.withDefault []

                    else
                        []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate zscores site isChw db) extraMsgs

        SkipActivity activity ->
            ( { model | skippedActivities = EverySet.insert activity model.skippedActivities }
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate zscores site isChw db) [ SetDialogState Nothing ]

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )
