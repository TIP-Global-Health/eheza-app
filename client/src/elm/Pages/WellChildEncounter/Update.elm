module Pages.WellChildEncounter.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (EncounterNote(..))
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.Utils
import Pages.WellChildEncounter.Model exposing (..)
import Pages.WellChildEncounter.Utils
import RemoteData
import ZScore.Model


update : NominalDate -> ZScore.Model.Model -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate zscores isChw db msg model =
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

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        TriggerAcuteIllnessEncounter childId encounterId ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (AcuteIllnessParticipantPage childId))
                    , SetWarningPopupState Nothing
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
                |> sequenceExtra (update currentDate zscores isChw db) extraMsgs

        NavigateToActivity encounterId activity ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (WellChildActivityPage encounterId activity)) ]

                -- View Assessment popu when mavigating to Next Steps activity.
                appMsgs =
                    if activity == WellChildNextSteps then
                        Pages.WellChildEncounter.Utils.generateAssembledData encounterId db
                            |> RemoteData.toMaybe
                            |> Maybe.map
                                (\assembled ->
                                    let
                                        mandatoryActivitiesCompleted =
                                            Pages.WellChildActivity.Utils.mandatoryNutritionAssessmentTasksCompleted
                                                currentDate
                                                isChw
                                                assembled
                                                db
                                    in
                                    if not mandatoryActivitiesCompleted then
                                        -- Assement is done only when all mandatory measurements were recorded.
                                        []

                                    else
                                        let
                                            assessment =
                                                Pages.WellChildActivity.Utils.generateNutritionAssessment currentDate zscores db assembled
                                        in
                                        if List.isEmpty assessment then
                                            []

                                        else
                                            -- Show warning popup with new assesment.
                                            Pages.WellChildActivity.Model.PopupNutritionAssessment assessment
                                                |> Just
                                                |> Pages.WellChildActivity.Model.SetWarningPopupState
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
                |> sequenceExtra (update currentDate zscores isChw db) extraMsgs
