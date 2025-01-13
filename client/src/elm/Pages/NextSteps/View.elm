module Pages.NextSteps.View exposing (view)

import Activity.Model exposing (Activity)
import Activity.Utils exposing (generateNutritionAssessment)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, mapMeasurementData)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (nutritionAssessmentForBackend)
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
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( viewContributingFactorsForm
        , viewHealthEducationForm
        , viewNutritionFollowUpForm
        , viewSendToHealthCenterForm
        )
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.NextSteps.Utils exposing (nextStepsTasksCompletedFromTotal)
import Pages.Nutrition.Activity.View exposing (warningPopup)
import Pages.Utils exposing (resolveNextTask, tasksBarId, viewPersonDetails, viewTasksCount)
import RemoteData
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
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
                (SetWarningPopupState [])
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

                    contributingFactorsValue =
                        getMeasurementValueFunc contributingFactors

                    followUp =
                        mapMeasurementData .followUp measurements
                            |> .current

                    followUpValue =
                        getMeasurementValueFunc followUp

                    sendToHC =
                        mapMeasurementData .sendToHC measurements
                            |> .current

                    sendToHCValue =
                        getMeasurementValueFunc sendToHC

                    healthEducation =
                        mapMeasurementData .healthEducation measurements
                            |> .current

                    healthEducationValue =
                        getMeasurementValueFunc healthEducation

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
                            [ div attributes
                                [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                                , text <| translate language (Translate.NutritionNextStepsTask task)
                                ]
                            ]

                    tasksCompletedFromTotalDict =
                        List.map (\task -> ( task, nextStepsTasksCompletedFromTotal currentDate measurements model task )) tasks
                            |> Dict.fromList

                    ( tasksCompleted, totalTasks ) =
                        Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                            |> Maybe.withDefault ( 0, 0 )

                    viewForm =
                        case activeTask of
                            Just NextStepsSendToHC ->
                                sendToHCFormWithDefault model.sendToHCForm sendToHCValue
                                    |> viewSendToHealthCenterForm language
                                        currentDate
                                        SetReferToHealthCenter
                                        SetReasonForNonReferral
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
                                nutritionFollowUpFormWithDefault model.followUpForm followUpValue
                                    |> viewNutritionFollowUpForm language currentDate SetFollowUpOption

                            Nothing ->
                                emptyNode

                    actions =
                        Maybe.map
                            (\task ->
                                let
                                    nextTask =
                                        resolveNextTask task tasksCompletedFromTotalDict tasks

                                    saveAction =
                                        case task of
                                            NextStepsSendToHC ->
                                                toSendToHCValueWithDefault sendToHCValue model.sendToHCForm
                                                    |> Maybe.map
                                                        (\value ->
                                                            let
                                                                sendToHCId =
                                                                    Maybe.map Tuple.first sendToHC
                                                            in
                                                            SaveSendToHC sendToHCId value nextTask |> onClick |> List.singleton
                                                        )
                                                    |> Maybe.withDefault []

                                            NextStepsHealthEducation ->
                                                toHealthEducationValueWithDefault healthEducationValue model.healthEducationForm
                                                    |> Maybe.map
                                                        (\value ->
                                                            let
                                                                healthEducationId =
                                                                    Maybe.map Tuple.first healthEducation
                                                            in
                                                            SaveHealthEducation healthEducationId value nextTask |> onClick |> List.singleton
                                                        )
                                                    |> Maybe.withDefault []

                                            NextStepContributingFactors ->
                                                toContributingFactorsValueWithDefault contributingFactorsValue model.contributingFactorsForm
                                                    |> Maybe.map
                                                        (\value ->
                                                            let
                                                                contributingFactorsId =
                                                                    Maybe.map Tuple.first contributingFactors
                                                            in
                                                            SaveContributingFactors contributingFactorsId value nextTask |> onClick |> List.singleton
                                                        )
                                                    |> Maybe.withDefault []

                                            NextStepFollowUp ->
                                                let
                                                    assesment =
                                                        generateNutritionAssessment currentDate zscores childId db session.offlineSession
                                                            |> nutritionAssessmentForBackend

                                                    form =
                                                        model.followUpForm |> (\form_ -> { form_ | assesment = Just assesment })
                                                in
                                                toNutritionFollowUpValueWithDefault followUpValue form
                                                    |> Maybe.map
                                                        (\value ->
                                                            let
                                                                followUpId =
                                                                    Maybe.map Tuple.first followUp
                                                            in
                                                            SaveFollowUp followUpId value nextTask |> onClick |> List.singleton
                                                        )
                                                    |> Maybe.withDefault []
                                in
                                div [ class "actions next-steps" ]
                                    [ button
                                        (classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ] :: saveAction)
                                        [ text <| translate language Translate.Save ]
                                    ]
                            )
                            activeTask
                            |> Maybe.withDefault emptyNode
                in
                [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
                    [ div [ class "ui three column grid" ] <|
                        List.map viewTask tasks
                    ]
                , viewTasksCount language tasksCompleted totalTasks
                , div [ class "ui full segment" ]
                    [ div [ class "full content" ]
                        [ viewForm
                        , actions
                        ]
                    ]
                ]
            )
