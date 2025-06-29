module Pages.Nutrition.Activity.View exposing
    ( translateNutritionAssement
    , view
    , viewPhotoForm
    , warningPopup
    )

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionEncounter.Utils
    exposing
        ( calculateZScoreWeightForAge
        , getNewbornExamPregnancySummary
        , nutritionAssessmentForBackend
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
        , resolvePreviousValuesSetForChild
        )
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model
    exposing
        ( DropZoneFile
        , HeightForm
        , MuacForm
        , NCDAData
        , NextStepsTask(..)
        , NutritionForm
        , WeightForm
        )
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( heightFormAndTasks
        , viewColorAlertIndication
        , viewContributingFactorsForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewNutritionFollowUpForm
        , viewNutritionForm
        , viewSendToHealthCenterForm
        , weightFormAndTasks
        , zScoreForHeightOrLength
        )
import Pages.Nutrition.Activity.Model exposing (..)
import Pages.Nutrition.Activity.Utils exposing (..)
import Pages.Nutrition.Encounter.Model exposing (AssembledData)
import Pages.Nutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , tasksBarId
        , viewCheckBoxMultipleSelectInput
        , viewLabel
        , viewMeasurementInput
        , viewPersonDetails
        , viewPhotoThumbFromImageUrl
        , viewPreviousMeasurement
        , viewSaveAction
        , viewTasksCount
        )
import SyncManager.Model exposing (Site(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..))
import ZScore.Utils exposing (diffDays, viewZScore, zScoreLengthHeightForAge)


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> NutritionEncounterId
    -> NutritionActivity
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site id activity isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site id activity isChw db model) identity assembled


viewHeaderAndContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> NutritionEncounterId
    -> NutritionActivity
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate zscores site id activity isChw db model assembled =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores site id activity isChw db model assembled
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


viewHeader : Language -> NutritionEncounterId -> NutritionActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.NutritionActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> NutritionEncounterId
    -> NutritionActivity
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate zscores site id activity isChw db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate zscores site id activity isChw assembled db model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> NominalDate -> msg -> List NutritionAssessment -> Maybe (Html msg)
warningPopup language currentDate closePopupMsg state =
    if List.isEmpty state then
        Nothing

    else
        let
            infoHeading =
                [ div [ class "popup-heading" ] [ text <| translate language Translate.Assessment ++ ":" ] ]

            assessments =
                List.map (\assessment -> p [] [ translateNutritionAssement language assessment ]) state
        in
        Just <|
            div [ class "ui active modal diagnosis-popup" ]
                [ div [ class "content" ] <|
                    [ div [ class "popup-heading-wrapper" ] infoHeading
                    , div [ class "popup-title" ] assessments
                    ]
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick closePopupMsg
                        ]
                        [ text <| translate language Translate.Continue ]
                    ]
                ]


translateNutritionAssement : Language -> NutritionAssessment -> Html any
translateNutritionAssement language assessment =
    case assessment of
        AssesmentMalnutritionSigns signs ->
            let
                translatedSigns =
                    List.map (Translate.ChildNutritionSignLabel >> translate language) signs
                        |> String.join ", "
            in
            text <| translate language (Translate.NutritionAssessment assessment) ++ ": " ++ translatedSigns

        _ ->
            text <| translate language <| Translate.NutritionAssessment assessment


viewActivity :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> NutritionEncounterId
    -> NutritionActivity
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> Model
    -> List (Html Msg)
viewActivity language currentDate zscores site id activity isChw assembled db model =
    let
        previousValuesSet =
            resolvePreviousValuesSetForChild currentDate site assembled.participant.person db
    in
    case activity of
        Height ->
            viewHeightContent language currentDate zscores isChw assembled model.heightData previousValuesSet.height

        Muac ->
            viewMuacContent language currentDate site assembled model.muacData previousValuesSet.muac

        Nutrition ->
            viewNutritionContent language currentDate zscores assembled db model.nutritionData

        Photo ->
            viewPhotoContent language currentDate ( assembled.participant.person, assembled.measurements ) model.photoData

        Weight ->
            viewWeightContent language currentDate zscores site isChw assembled model.weightData previousValuesSet.weight

        NCDA ->
            viewNCDAContent language currentDate zscores site id assembled model.ncdaData db

        NextSteps ->
            viewNextStepsContent language currentDate zscores id assembled db model.nextStepsData


viewHeightContent : Language -> NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> HeightData -> Maybe Float -> List (Html Msg)
viewHeightContent language currentDate zscores isChw assembled data previousValue =
    let
        form =
            getMeasurementValueFunc assembled.measurements.height
                |> heightFormWithDefault assembled.encounter.skippedForms data.form

        ( formForView, tasks ) =
            heightFormAndTasks language currentDate zscores isChw assembled.person previousValue SetHeight SetHeightNotTaken form

        totalTasks =
            List.length tasks

        tasksCompleted =
            List.map taskCompleted tasks
                |> List.sum

        constraints =
            getInputConstraintsHeight

        disabled =
            (form.measurementNotTaken /= Just True)
                && ((tasksCompleted /= totalTasks)
                        || (Maybe.map (withinConstraints constraints >> not) form.height
                                |> Maybe.withDefault True
                           )
                   )
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            formForView
        , viewSaveAction language
            (SaveHeight assembled.encounter.skippedForms assembled.participant.person assembled.measurements.height)
            disabled
        ]
    ]


viewMuacContent : Language -> NominalDate -> Site -> AssembledData -> MuacData -> Maybe Float -> List (Html Msg)
viewMuacContent language currentDate site assembled data previousValue =
    let
        form =
            getMeasurementValueFunc assembled.measurements.muac
                |> muacFormWithDefault data.form

        ( inputs, tasks ) =
            Measurement.View.muacFormInputsAndTasks language currentDate site assembled.person previousValue SetMuac form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        constraints =
            getInputConstraintsMuac site

        currentValue =
            case site of
                SiteBurundi ->
                    -- Value is stored in cm, but for Burundi, we need to
                    -- view it as mm. Therefore, multiplying by 10.
                    Maybe.map ((*) 10) form.muac

                _ ->
                    form.muac

        disabled =
            (tasksCompleted /= tasksTotal)
                || (Maybe.map (withinConstraints constraints >> not) currentValue
                        |> Maybe.withDefault True
                   )
    in
    [ viewTasksCount language tasksCompleted tasksTotal
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form muac" ]
                inputs
            ]
        , viewSaveAction language
            (SaveMuac assembled.participant.person assembled.measurements.muac)
            disabled
        ]
    ]


viewNutritionContent : Language -> NominalDate -> ZScore.Model.Model -> AssembledData -> ModelIndexedDb -> NutritionData -> List (Html Msg)
viewNutritionContent language currentDate zscores assembled db data =
    let
        form =
            getMeasurementValueFunc assembled.measurements.nutrition
                |> nutritionFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs

        assessment =
            generateNutritionAssessment currentDate zscores db assembled
                |> nutritionAssessmentForBackend
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewNutritionForm language currentDate SetNutritionSign form
        , viewSaveAction language
            (SaveNutrition assembled.participant.person assembled.measurements.nutrition assessment)
            (tasksCompleted /= totalTasks)
        ]
    ]


viewPhotoContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> PhotoData -> List (Html Msg)
viewPhotoContent language currentDate ( personId, measurements ) data =
    let
        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, disabled ) =
            case data.form.url of
                Just url ->
                    let
                        photoId =
                            Maybe.map Tuple.first measurements.photo
                    in
                    ( Just url
                    , SavePhoto personId photoId url
                    , False
                    )

                Nothing ->
                    ( getMeasurementValueFunc measurements.photo
                    , NoOp
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewPhotoForm language currentDate displayPhoto DropZoneComplete
        , viewSaveAction language saveMsg disabled
        ]
    ]


viewPhotoForm : Language -> NominalDate -> Maybe ImageUrl -> (DropZoneFile -> msg) -> List (Html msg)
viewPhotoForm language currentDate displayPhoto dropZoneCompleteMsg =
    let
        activity =
            Photo
    in
    [ divKeyed [ class "content" ]
        [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
            |> keyed "help"
        , keyedDivKeyed "grid"
            [ class "ui grid" ]
            [ Maybe.map viewPhotoThumbFromImageUrl displayPhoto
                |> showMaybe
                |> List.singleton
                |> div [ class "eight wide column" ]
                |> keyed "thumbnail"
            , div
                [ id "dropzone"
                , class "eight wide column dropzone"
                , on "dropzonecomplete" (Json.Decode.map dropZoneCompleteMsg decodeDropZoneFile)
                ]
                [ div
                    [ class "dz-message"
                    , attribute "data-dz-message" ""
                    ]
                    [ span
                        []
                        [ text <| translate language Translate.DropzoneDefaultMessage ]
                    ]
                ]
                |> keyed "dropzone"
            ]
        ]
    ]


viewWeightContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> Bool
    -> AssembledData
    -> WeightData
    -> Maybe Float
    -> List (Html Msg)
viewWeightContent language currentDate zscores site isChw assembled data previousValue =
    let
        form =
            getMeasurementValueFunc assembled.measurements.weight
                |> weightFormWithDefault assembled.encounter.skippedForms data.form

        ( formForView, tasks ) =
            weightFormAndTasks language
                currentDate
                zscores
                site
                isChw
                assembled.person
                heightValue
                previousValue
                True
                SetWeight
                SetWeightNotTaken
                form

        totalTasks =
            List.length tasks

        tasksCompleted =
            List.map taskCompleted tasks
                |> List.sum

        heightValue =
            getMeasurementValueFunc assembled.measurements.height

        constraints =
            getInputConstraintsWeight

        disabled =
            (form.measurementNotTaken /= Just True)
                && ((tasksCompleted /= totalTasks)
                        || (Maybe.map (withinConstraints constraints >> not) form.weight
                                |> Maybe.withDefault True
                           )
                   )
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            formForView
        , viewSaveAction language
            (SaveWeight assembled.encounter.skippedForms assembled.participant.person assembled.measurements.weight)
            disabled
        ]
    ]


viewNCDAContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> NutritionEncounterId
    -> AssembledData
    -> NCDAData
    -> ModelIndexedDb
    -> List (Html Msg)
viewNCDAContent language currentDate zscores site id assembled data db =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        config =
            { atHealthCenter = True
            , showTasksTray = True
            , pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate personId assembled.person db
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStuntingLevelMsg = SetStuntingLevel
            , setWeightMsg = SetWeightForNCDA
            , setMuacMsg = SetMuacForNCDA
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg = SaveNCDA personId assembled.measurements.ncda
            }
    in
    Measurement.View.viewNCDAContent language
        currentDate
        zscores
        site
        personId
        assembled.person
        config
        data.helperState
        form
        db


viewNextStepsContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> AssembledData -> ModelIndexedDb -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate zscores id assembled db data =
    let
        measurements =
            assembled.measurements

        tasks =
            allNextStepsTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

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
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, nextStepsTasksCompletedFromTotal currentDate measurements data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsSendToHC ->
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHealthCenterForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing

                Just NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation

                Just NextStepContributingFactors ->
                    getMeasurementValueFunc measurements.contributingFactors
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign

                Just NextStepFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> viewNutritionFollowUpForm language currentDate SetFollowUpOption

                Nothing ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                NextStepsSendToHC ->
                                    SaveSendToHC personId measurements.sendToHC nextTask

                                NextStepsHealthEducation ->
                                    SaveHealthEducation personId measurements.healthEducation nextTask

                                NextStepContributingFactors ->
                                    SaveContributingFactors personId measurements.contributingFactors nextTask

                                NextStepFollowUp ->
                                    let
                                        assesment =
                                            generateNutritionAssessment currentDate zscores db assembled
                                                |> nutritionAssessmentForBackend
                                    in
                                    SaveFollowUp personId measurements.followUp assesment nextTask
                    in
                    viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
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
