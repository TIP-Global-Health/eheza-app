module Pages.NextSteps.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..), emptySummaryByActivity)
import Activity.Utils exposing (generateNutritionAssesment, getActivityIcon, getAllActivities, getParticipantCountForActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (NutritionAssesment)
import Backend.Measurement.Utils exposing (mapMeasurementData)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (nutritionAssesmentForBackend)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildMeasurementData)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import LocalData
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (NextStepsTask(..))
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewSendToHCForm
        )
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.NextSteps.Utils exposing (nextStepsTasksCompletedFromTotal)
import Pages.NutritionActivity.Model exposing (NextStepsData)
import Pages.NutritionActivity.View exposing (warningPopup)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (isTaskCompleted, tasksBarId)
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
                |> Maybe.map (\child -> viewContent language currentDate zscores childId child session db model)
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


viewContent : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> EditableSession -> ModelIndexedDb -> Model -> Html Msg
viewContent language currentDate zscores childId child session db model =
    ((viewPersonDetails language currentDate child Nothing |> div [ class "item" ])
        :: viewNextStepsContent language currentDate zscores childId child session db model
    )
        |> div [ class "ui unstackable items" ]


viewNextStepsContent : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> EditableSession -> ModelIndexedDb -> Model -> List (Html Msg)
viewNextStepsContent language currentDate zscores childId child session db model =
    getChildMeasurementData childId session
        |> LocalData.unwrap
            []
            (\measurements ->
                let
                    tasks =
                        allNextStepsTasks

                    activeTask =
                        Maybe.Extra.or model.activeTask (List.head tasks)

                    contributingFactors =
                        mapMeasurementData .contributingFactors measurements
                            |> .current

                    contributingFactorsId =
                        Maybe.map Tuple.first contributingFactors

                    contributingFactorsValue =
                        Maybe.map (Tuple.second >> .value) contributingFactors

                    followUp =
                        mapMeasurementData .followUp measurements
                            |> .current

                    followUpId =
                        Maybe.map Tuple.first followUp

                    followUpValue =
                        Maybe.map (Tuple.second >> .value) followUp

                    sendToHC =
                        mapMeasurementData .sendToHC measurements
                            |> .current

                    sendToHCId =
                        Maybe.map Tuple.first sendToHC

                    sendToHCValue =
                        Maybe.map (Tuple.second >> .value) sendToHC

                    healthEducation =
                        mapMeasurementData .healthEducation measurements
                            |> .current

                    healthEducationId =
                        Maybe.map Tuple.first healthEducation

                    healthEducationValue =
                        Maybe.map (Tuple.second >> .value) healthEducation

                    viewTask task =
                        let
                            ( iconClass, isCompleted ) =
                                case task of
                                    NextStepsSendToHC ->
                                        ( "next-steps-send-to-hc"
                                        , isJust sendToHCValue
                                        )

                                    NextStepsHealthEducation ->
                                        ( "next-steps-health-education"
                                        , isJust healthEducationValue
                                        )

                                    NextStepContributingFactors ->
                                        ( "next-steps-contributing-factors"
                                        , isJust contributingFactorsValue
                                        )

                                    NextStepFollowUp ->
                                        ( "next-steps-follow-up"
                                        , isJust followUpValue
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

                    tasksCompletedFromTotalDict =
                        tasks
                            |> List.map (\task -> ( task, nextStepsTasksCompletedFromTotal measurements model task ))
                            |> Dict.fromList

                    ( tasksCompleted, totalTasks ) =
                        activeTask
                            |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                            |> Maybe.withDefault ( 0, 0 )

                    viewForm =
                        case activeTask of
                            Just NextStepsSendToHC ->
                                sendToHCFormWithDefault model.sendToHCForm sendToHCValue
                                    |> viewSendToHCForm language
                                        currentDate
                                        SetReferToHealthCenter
                                        SetReasonForNotSendingToHC
                                        SetHandReferralForm
                                        Nothing

                            Just NextStepsHealthEducation ->
                                healthEducationFormWithDefault model.healthEducationForm healthEducationValue
                                    |> viewHealthEducationForm language
                                        currentDate
                                        SetProvidedEducationForDiagnosis
                                        SetReasonForNotProvidingHealthEducation

                            Just NextStepContributingFactors ->
                                contributingFactorsFormWithDefault model.contributingFactorsForm contributingFactorsValue
                                    |> viewContributingFactorsForm language currentDate SetContributingFactorsSign

                            Just NextStepFollowUp ->
                                followUpFormWithDefault model.followUpForm followUpValue
                                    |> viewFollowUpForm language currentDate SetFollowUpOption

                            Nothing ->
                                emptyNode

                    nextTask =
                        List.filter
                            (\task ->
                                (Just task /= activeTask)
                                    && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                            )
                            tasks
                            |> List.head

                    actions =
                        activeTask
                            |> Maybe.map
                                (\task ->
                                    let
                                        saveAction =
                                            case task of
                                                NextStepsSendToHC ->
                                                    toSendToHCValueWithDefault sendToHCValue model.sendToHCForm
                                                        |> Maybe.map (\value -> SaveSendToHC sendToHCId value nextTask |> onClick |> List.singleton)
                                                        |> Maybe.withDefault []

                                                NextStepsHealthEducation ->
                                                    toHealthEducationValueWithDefault healthEducationValue model.healthEducationForm
                                                        |> Maybe.map (\value -> SaveHealthEducation healthEducationId value nextTask |> onClick |> List.singleton)
                                                        |> Maybe.withDefault []

                                                NextStepContributingFactors ->
                                                    toContributingFactorsValueWithDefault contributingFactorsValue model.contributingFactorsForm
                                                        |> Maybe.map (\value -> SaveContributingFactors contributingFactorsId value nextTask |> onClick |> List.singleton)
                                                        |> Maybe.withDefault []

                                                NextStepFollowUp ->
                                                    let
                                                        assesment =
                                                            generateNutritionAssesment currentDate zscores childId db session.offlineSession
                                                                |> nutritionAssesmentForBackend

                                                        form =
                                                            model.followUpForm |> (\form_ -> { form_ | assesment = Just assesment })
                                                    in
                                                    toFollowUpValueWithDefault followUpValue form
                                                        |> Maybe.map (\value -> SaveFollowUp followUpId value nextTask |> onClick |> List.singleton)
                                                        |> Maybe.withDefault []
                                    in
                                    div [ class "actions next-steps" ]
                                        [ button
                                            (classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ] :: saveAction)
                                            [ text <| translate language Translate.Save ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode
                in
                [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
                    [ div [ class "ui three column grid" ] <|
                        List.map viewTask tasks
                    ]
                , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
                , div [ class "ui full segment" ]
                    [ div [ class "full content" ]
                        [ viewForm
                        , actions
                        ]
                    ]
                ]
            )
