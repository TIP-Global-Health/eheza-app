module Pages.Nutrition.Activity.View exposing
    ( translateNutritionAssement
    , view
    , viewHeightForm
    , viewMuacForm
    , viewNutritionForm
    , viewPhotoForm
    , viewWeightForm
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
import EverySet exposing (EverySet)
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
        ( viewColorAlertIndication
        , viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewSendToHealthCenterForm
        , zScoreForHeightOrLength
        )
import Pages.Nutrition.Activity.Model exposing (..)
import Pages.Nutrition.Activity.Utils exposing (..)
import Pages.Nutrition.Encounter.Model exposing (AssembledData)
import Pages.Nutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewCheckBoxMultipleSelectInput
        , viewLabel
        , viewMeasurementInput
        , viewPersonDetails
        , viewPhotoThumbFromImageUrl
        , viewPreviousMeasurement
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
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site id activity isChw db model) identity data


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
viewHeaderAndContent language currentDate zscores site id activity isChw db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores site id activity isChw db model data
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
            , span [] []
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
            resolvePreviousValuesSetForChild currentDate assembled.participant.person db
    in
    case activity of
        Height ->
            viewHeightContent language currentDate zscores assembled model.heightData previousValuesSet.height

        Muac ->
            viewMuacContent language currentDate site assembled model.muacData previousValuesSet.muac

        Nutrition ->
            viewNutritionContent language currentDate zscores assembled db model.nutritionData

        Photo ->
            viewPhotoContent language currentDate ( assembled.participant.person, assembled.measurements ) model.photoData

        Weight ->
            viewWeightContent language currentDate zscores assembled model.weightData previousValuesSet.weight

        NCDA ->
            viewNCDAContent language currentDate zscores id assembled model.ncdaData db

        NextSteps ->
            viewNextStepsContent language currentDate zscores id assembled db model.nextStepsData


viewHeightContent : Language -> NominalDate -> ZScore.Model.Model -> AssembledData -> HeightData -> Maybe Float -> List (Html Msg)
viewHeightContent language currentDate zscores assembled data previousValue =
    let
        form =
            assembled.measurements.height
                |> getMeasurementValueFunc
                |> heightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.height

        constraints =
            getInputConstraintsHeight

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.height
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewHeightForm language currentDate zscores assembled.person previousValue SetHeight form
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveHeight assembled.participant.person assembled.measurements.height
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHeightForm :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe Float
    -> (String -> msg)
    -> HeightForm
    -> List (Html msg)
viewHeightForm language currentDate zscores person previousValue setHeightMsg form =
    let
        activity =
            Height

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreText =
            form.height
                |> Maybe.andThen
                    (\height ->
                        Maybe.andThen
                            (\ageInDays ->
                                zScoreLengthHeightForAge zscores ageInDays person.gender (Centimetres height)
                            )
                            maybeAgeInDays
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsHeight
    in
    [ div [ class "ui form height" ]
        [ viewLabel language <| Translate.NutritionActivityTitle activity
        , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
        , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.height
                    setHeightMsg
                    "height"
                    Translate.CentimeterShorthand
                ]
            , div
                [ class "five wide column" ]
                [ showMaybe <|
                    Maybe.map2 (viewMeasurementFloatDiff language Translate.CentimeterShorthand)
                        form.height
                        previousValue
                ]
            ]
        , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
        ]
    , div [ class "ui large header z-score age" ]
        [ text <| translate language Translate.ZScoreHeightForAge
        , span [ class "sub header" ]
            [ text zScoreText ]
        ]
    ]


viewMuacContent : Language -> NominalDate -> Site -> AssembledData -> MuacData -> Maybe Float -> List (Html Msg)
viewMuacContent language currentDate site assembled data previousValue =
    let
        form =
            assembled.measurements.muac
                |> getMeasurementValueFunc
                |> muacFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.muac

        constraints =
            getInputConstraintsMuac

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.muac
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewMuacForm language currentDate site assembled.person previousValue SetMuac form
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveMuac assembled.participant.person assembled.measurements.muac
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewMuacForm :
    Language
    -> NominalDate
    -> Site
    -> Person
    -> Maybe Float
    -> (String -> msg)
    -> MuacForm
    -> List (Html msg)
viewMuacForm language currentDate site person previousValue setMuacMsg form =
    let
        activity =
            Muac

        constraints =
            getInputConstraintsMuac
    in
    [ div [ class "ui form muac" ]
        [ viewLabel language <| Translate.NutritionActivityTitle activity
        , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
        , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.muac
                    setMuacMsg
                    "muac"
                    Translate.CentimeterShorthand
                ]
            , div
                [ class "five wide column" ]
                [ showMaybe <|
                    Maybe.map (MuacInCm >> muacIndication >> viewColorAlertIndication language) form.muac
                ]
            ]
        , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
        ]
    ]


viewNutritionContent : Language -> NominalDate -> ZScore.Model.Model -> AssembledData -> ModelIndexedDb -> NutritionData -> List (Html Msg)
viewNutritionContent language currentDate zscores assembled db data =
    let
        form =
            assembled.measurements.nutrition
                |> getMeasurementValueFunc
                |> nutritionFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs

        assessment =
            generateNutritionAssessment currentDate zscores db assembled
                |> nutritionAssessmentForBackend
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewNutritionForm language currentDate SetNutritionSign form
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveNutrition assembled.participant.person assembled.measurements.nutrition assessment
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewNutritionForm : Language -> NominalDate -> (ChildNutritionSign -> msg) -> NutritionForm -> List (Html msg)
viewNutritionForm language currentDate setSignMsg form =
    let
        activity =
            Nutrition
    in
    [ div [ class "ui form nutrition" ]
        [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
        , viewLabel language Translate.SelectAllSigns
        , viewCheckBoxMultipleSelectInput language
            [ Edema, AbdominalDistension, DrySkin ]
            [ Apathy, PoorAppetite, BrittleHair ]
            (form.signs |> Maybe.withDefault [])
            (Just NormalChildNutrition)
            setSignMsg
            Translate.ChildNutritionSignLabel
        ]
    ]


viewPhotoContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> PhotoData -> List (Html Msg)
viewPhotoContent language currentDate ( personId, measurements ) data =
    let
        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case data.form.url of
                Just url ->
                    let
                        photoId =
                            Maybe.map Tuple.first measurements.photo
                    in
                    ( Just url
                    , [ onClick <| SavePhoto personId photoId url ]
                    , False
                    )

                Nothing ->
                    ( getMeasurementValueFunc measurements.photo
                    , []
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewPhotoForm language currentDate displayPhoto DropZoneComplete
        , div [ class "actions" ]
            [ button
                (classList
                    [ ( "ui fluid primary button", True )
                    , ( "disabled", isDisabled )
                    ]
                    :: saveMsg
                )
                [ text <| translate language Translate.Save ]
            ]
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


viewWeightContent : Language -> NominalDate -> ZScore.Model.Model -> AssembledData -> WeightData -> Maybe Float -> List (Html Msg)
viewWeightContent language currentDate zscores assembled data previousValue =
    let
        form =
            assembled.measurements.weight
                |> getMeasurementValueFunc
                |> weightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.weight

        heightValue =
            assembled.measurements.height
                |> getMeasurementValueFunc

        constraints =
            getInputConstraintsWeight

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.weight
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewWeightForm language currentDate zscores assembled.person heightValue previousValue True SetWeight form
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveWeight assembled.participant.person assembled.measurements.weight
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewWeightForm :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe HeightInCm
    -> Maybe Float
    -> Bool
    -> (String -> msg)
    -> WeightForm
    -> List (Html msg)
viewWeightForm language currentDate zscores person heightValue previousValue showWeightForHeightZScore setWeightMsg form =
    let
        activity =
            Weight

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreForAgeText =
            calculateZScoreWeightForAge currentDate zscores person form.weight
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        zScoreForHeightText =
            heightValue
                |> Maybe.andThen
                    (\(HeightInCm height) ->
                        form.weight
                            |> Maybe.andThen
                                (\weight ->
                                    Maybe.andThen
                                        (\ageInDays ->
                                            zScoreForHeightOrLength zscores ageInDays (Centimetres height) person.gender weight
                                        )
                                        maybeAgeInDays
                                )
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsWeight
    in
    [ div [ class "ui form weight" ]
        [ viewLabel language <| Translate.NutritionActivityTitle activity
        , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
        , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.weight
                    setWeightMsg
                    "weight"
                    Translate.KilogramShorthand
                ]
            , div
                [ class "five wide column" ]
                [ showMaybe <|
                    Maybe.map2 (viewMeasurementFloatDiff language Translate.KilogramShorthand)
                        form.weight
                        previousValue
                ]
            ]
        , viewPreviousMeasurement language previousValue Translate.KilogramShorthand
        ]
    , div [ class "ui large header z-score age" ]
        [ text <| translate language Translate.ZScoreWeightForAge
        , span [ class "sub header" ]
            [ text zScoreForAgeText ]
        ]
    , showIf showWeightForHeightZScore <|
        div [ class "ui large header z-score height" ]
            [ text <| translate language Translate.ZScoreWeightForHeight
            , span [ class "sub header" ]
                [ text zScoreForHeightText
                ]
            ]
    ]


viewNCDAContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> NutritionEncounterId
    -> AssembledData
    -> NCDAData
    -> ModelIndexedDb
    -> List (Html Msg)
viewNCDAContent language currentDate zscores id assembled data db =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        config =
            { atHealthCenter = True
            , showTasksTray = True
            , behindOnVaccinations = Nothing
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
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, nextStepsTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsSendToHC ->
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHealthCenterForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing

                Just NextStepsHealthEducation ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation

                Just NextStepContributingFactors ->
                    measurements.contributingFactors
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign

                Just NextStepFollowUp ->
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
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
                            personId =
                                assembled.participant.person

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
                        div [ class "actions next-steps" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                , onClick saveMsg
                                ]
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
