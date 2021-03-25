module Pages.NextSteps.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..), emptySummaryByActivity)
import Activity.Utils exposing (getActivityIcon, getAllActivities, getParticipantCountForActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildMeasurementData)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import LocalData
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (NextStepsTask(..))
import Measurement.Utils exposing (allNextStepsTasks)
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.NutritionActivity.Model exposing (NextStepsData)
import Pages.NutritionActivity.Utils exposing (nextStepsTasksCompletedFromTotal)
import Pages.NutritionActivity.View exposing (warningPopup)
import Pages.NutritionEncounter.Model exposing (NutritionAssesment)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (backFromSessionPage, viewEndEncounterDialog)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (tabItem, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Activity -> ( SessionId, EditableSession ) -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores childId originActivity ( sessionId, session ) db model =
    let
        header =
            viewHeader language

        content =
            Dict.get childId db.people
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (viewContent language currentDate zscores childId)
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-activity nutrition" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.NextSteps ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Html Msg
viewContent language currentDate zscores childId child =
    ((viewPersonDetails language currentDate child Nothing |> div [ class "item" ])
        :: []
     -- viewActivity language currentDate zscores id activity isChw assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewNextStepsContent : Language -> NominalDate -> PersonId -> Person -> EditableSession -> Model -> List (Html Msg)
viewNextStepsContent language currentDate childId child session model =
    getChildMeasurementData childId session
        |> LocalData.unwrap
            []
            (\measurements ->
                let
                    tasks =
                        allNextStepsTasks

                    activeTask =
                        Maybe.Extra.or data.activeTask (List.head tasks)

                    viewTask task =
                        let
                            ( iconClass, isCompleted ) =
                                case task of
                                    NextStepsSendToHC ->
                                        ( "next-steps-send-to-hc"
                                        , isJust measurements.sendToHC
                                        )

                                    NextStepsHealthEducation ->
                                        ( "next-steps-health-education"
                                        , isJust measurements.healthEducation
                                        )

                                    NextStepContributingFactors ->
                                        ( "next-steps-contributing-factors"
                                        , isJust measurements.contributingFactors
                                        )

                                    NextStepFollowUp ->
                                        ( "next-steps-follow-up"
                                        , isJust measurements.followUp
                                        )

                            isActive =
                                activeTask == Just task

                            attributes =
                                classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                                    :: (if isActive then
                                            []

                                        else
                                            [ onClick <| SetActiveNextStepsTask task ]
                                       )
                        in
                        div [ class "column" ]
                            [ a attributes
                                [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                                , text <| translate language (Translate.NutritionNextStepsTask task)
                                ]
                            ]

                    data =
                        NextStepsData model.sendToHCForm model.healthEducationForm model.contributingFactorsForm model.followUpForm Nothing

                    tasksCompletedFromTotalDict =
                        tasks
                            |> List.map (\task -> ( task, nextStepsTasksCompletedFromTotal measurements data task ))
                            |> Dict.fromList

                    ( tasksCompleted, totalTasks ) =
                        activeTask
                            |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                            |> Maybe.withDefault ( 0, 0 )
                in
                []
            )



--     viewForm =
--         case activeTask of
--             Just NextStepsSendToHC ->
--                 measurements.sendToHC
--                     |> Maybe.map (Tuple.second >> .value)
--                     |> sendToHCFormWithDefault data.sendToHCForm
--                     |> viewSendToHCForm language
--                         currentDate
--                         SetReferToHealthCenter
--                         SetReasonForNotSendingToHC
--                         SetHandReferralForm
--
--             Just NextStepsHealthEducation ->
--                 measurements.healthEducation
--                     |> Maybe.map (Tuple.second >> .value)
--                     |> healthEducationFormWithDefault data.healthEducationForm
--                     |> viewHealthEducationForm language
--                         currentDate
--                         SetProvidedEducationForDiagnosis
--                         SetReasonForNotProvidingHealthEducation
--
--             Just NextStepContributingFactors ->
--                 measurements.contributingFactors
--                     |> Maybe.map (Tuple.second >> .value)
--                     |> contributingFactorsFormWithDefault data.contributingFactorsForm
--                     |> viewContributingFactorsForm language currentDate SetContributingFactorsSign
--
--             Just NextStepFollowUp ->
--                 measurements.followUp
--                     |> Maybe.map (Tuple.second >> .value)
--                     |> followUpFormWithDefault data.followUpForm
--                     |> viewFollowUpForm language currentDate SetFollowUpOption
--
--             Nothing ->
--                 emptyNode
--
--     nextTask =
--         List.filter
--             (\task ->
--                 (Just task /= activeTask)
--                     && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
--             )
--             tasks
--             |> List.head
--
--     actions =
--         activeTask
--             |> Maybe.map
--                 (\task ->
--                     let
--                         saveMsg =
--                             case task of
--                                 NextStepsSendToHC ->
--                                     SaveSendToHC personId measurements.sendToHC nextTask
--
--                                 NextStepsHealthEducation ->
--                                     SaveHealthEducation personId measurements.healthEducation nextTask
--
--                                 NextStepContributingFactors ->
--                                     SaveContributingFactors personId measurements.contributingFactors nextTask
--
--                                 NextStepFollowUp ->
--                                     SaveFollowUp personId measurements.followUp nextTask
--                     in
--                     div [ class "actions next-steps" ]
--                         [ button
--                             [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
--                             , onClick saveMsg
--                             ]
--                             [ text <| translate language Translate.Save ]
--                         ]
--                 )
--             |> Maybe.withDefault emptyNode
-- in
-- [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
--     [ div [ class "ui three column grid" ] <|
--         List.map viewTask tasks
--     ]
-- , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
-- , div [ class "ui full segment" ]
--     [ div [ class "full content" ]
--         [ viewForm
--         , actions
--         ]
--     ]
-- ]
