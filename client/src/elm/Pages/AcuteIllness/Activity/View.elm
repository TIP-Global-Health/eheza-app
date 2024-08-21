module Pages.AcuteIllness.Activity.View exposing
    ( view
    , viewAdministeredMedicationCustomLabel
    , viewAdministeredMedicationLabel
    , viewAdministeredMedicationQuestion
    , viewAmoxicillinAdministrationInstructions
    , viewHCRecommendation
    , viewOralSolutionPrescription
    , viewParacetamolAdministrationInstructions
    , viewTabletsPrescription
    )

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (malariaRapidTestResultAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (covidIsolationPeriod, getMeasurementValueFunc, muacValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (defaultIconForPerson, generateFullName, isPersonAFertileWoman)
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Form
import Form.Input
import GeoLocation.Model exposing (GeoInfo)
import GeoLocation.Utils exposing (..)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Model
    exposing
        ( HealthEducationForm
        , InvokationModule(..)
        , OngoingTreatmentReviewForm
        , VitalsForm
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( healthEducationFormWithDefault
        , muacFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        , vitalsFormWithDefault
        )
import Measurement.View exposing (renderDatePart, viewSendToHealthCenterForm, viewSendToHospitalForm)
import Pages.AcuteIllness.Activity.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.AcuteIllness.Activity.Utils exposing (..)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (..)
import Pages.AcuteIllness.Encounter.View exposing (viewPersonDetailsWithAlert, warningPopup)
import Pages.Nutrition.Activity.View exposing (viewMuacForm)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.View
import Pages.Utils
    exposing
        ( getCurrentReasonForMedicationNonAdministration
        , isTaskCompleted
        , nonAdministrationReasonToSign
        , resolveActiveTask
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCheckBoxValueInput
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewInstructionsLabel
        , viewLabel
        , viewQuestionLabel
        , viewRedAlertForSelect
        , viewTextInput
        )
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Form exposing (getValueAsInt, isFormFieldSet, viewFormError)
import Utils.Html exposing (thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewError, viewWebData)


view :
    Language
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> GeoInfo
    -> AcuteIllnessEncounterId
    -> Bool
    -> AcuteIllnessActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site features geoInfo id isChw activity db model =
    let
        assembled =
            generateAssembledData currentDate features id isChw db
    in
    viewWebData language (viewHeaderAndContent language currentDate site geoInfo id isChw activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Site -> GeoInfo -> AcuteIllnessEncounterId -> Bool -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate site geoInfo id isChw activity db model assembled =
    div [ class "page-activity acute-illness" ]
        [ viewHeader language id activity <| Maybe.map Tuple.second assembled.diagnosis
        , viewContent language currentDate site geoInfo id isChw activity db model assembled
        , viewModal <|
            warningPopup language
                currentDate
                isChw
                model.warningPopupState
                SetWarningPopupState
                assembled
        , viewModal <|
            pertinentSymptomsPopup language
                model.showPertinentSymptomsPopup
                (SetPertinentSymptomsPopupState False)
                assembled.measurements
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Maybe AcuteIllnessDiagnosis -> Html Msg
viewHeader language id activity diagnosis =
    let
        title =
            case activity of
                AcuteIllnessNextSteps ->
                    let
                        prefix =
                            diagnosis
                                |> Maybe.map (Translate.AcuteIllnessDiagnosis >> translate language >> (\diagnosisTitle -> diagnosisTitle ++ ": "))
                                |> Maybe.withDefault ""
                    in
                    prefix ++ translate language (Translate.AcuteIllnessActivityTitle activity)

                _ ->
                    translate language <| Translate.AcuteIllnessActivityTitle activity
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text title ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Site -> GeoInfo -> AcuteIllnessEncounterId -> Bool -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate site geoInfo id isChw activity db model assembled =
    (viewPersonDetailsWithAlert language currentDate isChw assembled model.showAlertsDialog SetAlertsDialogState
        :: viewActivity language currentDate site geoInfo id isChw activity db assembled model
    )
        |> div [ class "ui unstackable items" ]


pertinentSymptomsPopup : Language -> Bool -> msg -> AcuteIllnessMeasurements -> Maybe (Html msg)
pertinentSymptomsPopup language isOpen closeMsg measurements =
    if isOpen then
        let
            sectionLabel title =
                div [ class "section-label-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "section-label" ] [ text <| translate language title ++ ":" ]
                    ]

            viewLabelValuePopupItem transLabel value =
                translate language transLabel
                    ++ ": "
                    ++ value
                    |> viewPopupItem

            viewPopupItem value =
                div [ class "popup-item" ] [ text <| "- " ++ value ]

            vitalsValue =
                measurements.vitals
                    |> getMeasurementValueFunc

            viewBodyTemperature =
                vitalsValue
                    |> Maybe.map
                        (.bodyTemperature
                            >> (\bodyTemperature ->
                                    viewLabelValuePopupItem
                                        Translate.BodyTemperature
                                        (String.fromFloat bodyTemperature ++ " " ++ translate language Translate.CelsiusAbbrev)
                               )
                        )

            viewRespiratoryRate =
                vitalsValue
                    |> Maybe.map
                        (.respiratoryRate
                            >> (\respiratoryRate ->
                                    viewLabelValuePopupItem
                                        Translate.RespiratoryRate
                                        (String.fromInt respiratoryRate ++ " " ++ translate language Translate.BreathsPerMinuteUnitLabel)
                               )
                        )

            travelHistory =
                measurements.travelHistory
                    |> Maybe.map
                        (Tuple.second
                            >> .value
                            >> EverySet.member COVID19Country
                            >> (\isMember ->
                                    if isMember then
                                        Translate.Yes

                                    else
                                        Translate.No
                               )
                        )

            viewTravelHistory =
                travelHistory
                    |> Maybe.map (translate language >> viewLabelValuePopupItem Translate.TravelHistory)

            contactExposure =
                measurements.exposure
                    |> Maybe.map
                        (Tuple.second
                            >> .value
                            >> EverySet.member COVID19Symptoms
                            >> (\isMember ->
                                    if isMember then
                                        Translate.Yes

                                    else
                                        Translate.No
                               )
                        )

            viewContactExposure =
                contactExposure
                    |> Maybe.map (translate language >> viewLabelValuePopupItem Translate.ContactExposure)

            viewMalariaTesting =
                measurements.malariaTesting
                    |> Maybe.map
                        (Tuple.second
                            >> .value
                            >> (Translate.RapidTestResult
                                    >> translate language
                                    >> viewLabelValuePopupItem Translate.MalariaRapidDiagnosticTest
                               )
                        )

            symptomsGeneral =
                measurements.symptomsGeneral
                    |> Maybe.map
                        (Tuple.second
                            >> .value
                            >> Dict.toList
                            >> List.filterMap
                                (\( key, value ) ->
                                    if key /= NoSymptomsGeneral && value > 0 then
                                        viewLabelValuePopupItem
                                            (Translate.SymptomsGeneralSign key)
                                            (translate language <| Translate.DaysSinglePlural value)
                                            |> Just

                                    else
                                        Nothing
                                )
                        )
                    |> Maybe.withDefault []

            symptomsRespiratory =
                measurements.symptomsRespiratory
                    |> Maybe.map
                        (Tuple.second
                            >> .value
                            >> Dict.toList
                            >> List.filterMap
                                (\( key, value ) ->
                                    if key /= NoSymptomsRespiratory && value > 0 then
                                        viewLabelValuePopupItem
                                            (Translate.SymptomsRespiratorySign key)
                                            (translate language <| Translate.DaysSinglePlural value)
                                            |> Just

                                    else
                                        Nothing
                                )
                        )
                    |> Maybe.withDefault []

            symptomsGIValue =
                measurements.symptomsGI
                    |> getMeasurementValueFunc

            symptomsGI =
                symptomsGIValue
                    |> Maybe.map
                        (.signs
                            >> Dict.toList
                            >> List.filterMap
                                (\( key, value ) ->
                                    if key /= NoSymptomsGI && value > 0 then
                                        viewLabelValuePopupItem
                                            (Translate.SymptomsGISign key)
                                            (translate language <| Translate.DaysSinglePlural value)
                                            |> Just

                                    else
                                        Nothing
                                )
                        )
                    |> Maybe.withDefault []

            intractableVomiting =
                symptomsGIValue
                    |> Maybe.map
                        (.derivedSigns
                            >> EverySet.member IntractableVomiting
                            >> (\isMember ->
                                    if vomitingAtSymptoms measurements then
                                        viewPopupItem
                                            (translate language <| Translate.IntractableVomiting isMember)
                                            |> List.singleton

                                    else
                                        []
                               )
                        )
                    |> Maybe.withDefault []

            acuteFindingsValue =
                measurements.acuteFindings
                    |> getMeasurementValueFunc

            acuteFindingsGeneral =
                acuteFindingsValue
                    |> Maybe.map
                        (.signsGeneral
                            >> EverySet.toList
                            >> List.filter ((/=) NoAcuteFindingsGeneralSigns)
                            >> List.map
                                (Translate.AcuteFindingsGeneralSign
                                    >> translate language
                                    >> viewPopupItem
                                )
                        )
                    |> Maybe.withDefault []

            acuteFindingsRespiratory =
                acuteFindingsValue
                    |> Maybe.map
                        (.signsRespiratory
                            >> EverySet.toList
                            >> List.filter ((/=) NoAcuteFindingsRespiratorySigns)
                            >> List.map
                                (Translate.AcuteFindingsRespiratorySign
                                    >> translate language
                                    >> viewPopupItem
                                )
                        )
                    |> Maybe.withDefault []

            content =
                Maybe.Extra.values
                    [ viewBodyTemperature
                    , viewRespiratoryRate
                    , viewTravelHistory
                    , viewContactExposure
                    , viewMalariaTesting
                    ]
                    ++ symptomsGeneral
                    ++ symptomsRespiratory
                    ++ symptomsGI
                    ++ intractableVomiting
                    ++ acuteFindingsGeneral
                    ++ acuteFindingsRespiratory
        in
        Just <|
            div [ class "ui active modal alerts-dialog" ]
                [ div [ class "content" ]
                    [ div [ class "perinent-symptoms" ]
                        [ sectionLabel Translate.PertinentSymptoms
                        , content
                            |> div [ class "section-items" ]
                        ]
                    ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick closeMsg
                        ]
                        [ text <| translate language Translate.Close ]
                    ]
                ]

    else
        Nothing


viewActivity : Language -> NominalDate -> Site -> GeoInfo -> AcuteIllnessEncounterId -> Bool -> AcuteIllnessActivity -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate site geoInfo id isChw activity db assembled model =
    let
        personId =
            assembled.participant.person

        measurements =
            assembled.measurements
    in
    case activity of
        AcuteIllnessSymptoms ->
            viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) model.symptomsData

        AcuteIllnessPhysicalExam ->
            viewAcuteIllnessPhysicalExam language currentDate site id isChw assembled model.physicalExamData

        AcuteIllnessPriorTreatment ->
            viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) model.priorTreatmentData

        AcuteIllnessLaboratory ->
            viewAcuteIllnessLaboratory language currentDate id isChw assembled model.laboratoryData

        AcuteIllnessExposure ->
            viewAcuteIllnessExposure language currentDate id ( personId, measurements ) model.exposureData

        AcuteIllnessNextSteps ->
            viewAcuteIllnessNextSteps language currentDate site geoInfo id isChw assembled db model.nextStepsData

        AcuteIllnessOngoingTreatment ->
            viewAcuteIllnessOngoingTreatment language currentDate id ( personId, measurements ) model.ongoingTreatmentData

        AcuteIllnessDangerSigns ->
            viewAcuteIllnessDangerSigns language currentDate id ( personId, measurements ) model.dangerSignsData


viewAcuteIllnessSymptomsContent : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> SymptomsData -> List (Html Msg)
viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ SymptomsGeneral, SymptomsRespiratory, SymptomsGI ]

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        SymptomsGeneral ->
                            ( "symptoms-general", isJust measurements.symptomsGeneral )

                        SymptomsRespiratory ->
                            ( "symptoms-respiratory", isJust measurements.symptomsRespiratory )

                        SymptomsGI ->
                            ( "symptoms-gi", isJust measurements.symptomsGI )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveSymptomsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.SymptomsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, symptomsTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just SymptomsGeneral ->
                    measurements.symptomsGeneral
                        |> getMeasurementValueFunc
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
                        |> viewSymptomsGeneralForm language currentDate measurements

                Just SymptomsRespiratory ->
                    measurements.symptomsRespiratory
                        |> getMeasurementValueFunc
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
                        |> viewSymptomsRespiratoryForm language currentDate measurements

                Just SymptomsGI ->
                    measurements.symptomsGI
                        |> getMeasurementValueFunc
                        |> symptomsGIFormWithDefault data.symptomsGIForm
                        |> viewSymptomsGIForm language currentDate measurements

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
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                SymptomsGeneral ->
                                    SaveSymptomsGeneral personId measurements.symptomsGeneral nextTask

                                SymptomsRespiratory ->
                                    SaveSymptomsRespiratory personId measurements.symptomsRespiratory nextTask

                                SymptomsGI ->
                                    SaveSymptomsGI personId measurements.symptomsGI nextTask
                    in
                    div [ class "actions symptoms" ]
                        [ button
                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                            , onClick saveMsg
                            ]
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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewSymptomsGeneralForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsGeneralForm -> Html Msg
viewSymptomsGeneralForm language currentDate measurements form =
    viewCheckBoxValueInput language
        allSymptomsGeneralSigns
        form.signs
        ToggleSymptomsGeneralSign
        SetSymptomsGeneralSignValue
        Translate.SymptomsGeneralSign
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form general" ]


viewSymptomsRespiratoryForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsRespiratoryForm -> Html Msg
viewSymptomsRespiratoryForm language currentDate measurements form =
    let
        respiratorySignsWithoutCough =
            ( Tuple.first allSymptomsRespiratorySigns
                |> List.filter ((/=) Cough)
            , Tuple.second allSymptomsRespiratorySigns
            )
    in
    viewCheckBoxValueInput language
        respiratorySignsWithoutCough
        form.signs
        ToggleSymptomsRespiratorySign
        SetSymptomsRespiratorySignValue
        Translate.SymptomsRespiratorySign
        |> List.append (viewCoughInputItem language form.signs)
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form respiratory" ]


viewCoughInputItem : Language -> Dict SymptomsRespiratorySign Int -> List (Html Msg)
viewCoughInputItem language data =
    let
        currentValue =
            Dict.get Cough data

        isChecked =
            isJust currentValue

        periodSection =
            Maybe.map
                (\value ->
                    let
                        valueForInput =
                            if value == coughLessThan2WeeksConstant then
                                Just False

                            else if value == symptomMaxDuration then
                                Just True

                            else
                                Nothing
                    in
                    viewBoolInput
                        language
                        valueForInput
                        SetSymptomsRespiratoryCough
                        "cough-period"
                        (Just ( Translate.PeriodMoreThan2Weeks, Translate.Period2WeeksOrLess ))
                )
                currentValue
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui grid" ]
        [ div [ class "six wide column" ]
            [ div
                [ class "ui checkbox activity"
                , onClick <| ToggleSymptomsRespiratorySign Cough
                ]
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]
                    []
                , label []
                    [ text <| translate language (Translate.SymptomsRespiratorySign Cough) ]
                ]
            ]
        ]
    , periodSection
    ]


viewSymptomsGIForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsGIForm -> Html Msg
viewSymptomsGIForm language currentDate measurements form =
    let
        symptoms =
            viewCheckBoxValueInput language
                allSymptomsGISigns
                form.signs
                ToggleSymptomsGISign
                SetSymptomsGISignValue
                Translate.SymptomsGISign
                |> List.append
                    [ viewQuestionLabel language Translate.PatientGotAnySymptoms
                    , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
                    ]

        derivedQuestions =
            if Dict.member Vomiting form.signs then
                [ viewQuestionLabel language Translate.IntractableVomitingQuestion
                , viewBoolInput language
                    form.intractableVomiting
                    SetSymptomsGIIntractableVomiting
                    "intractable-vomiting"
                    Nothing
                ]

            else
                []
    in
    div [ class "symptoms-form gi" ]
        [ div [ class "symptoms" ] symptoms
        , div [ class "derived-questions" ] derivedQuestions
        ]


viewAcuteIllnessPhysicalExam :
    Language
    -> NominalDate
    -> Site
    -> AcuteIllnessEncounterId
    -> Bool
    -> AssembledData
    -> PhysicalExamData
    -> List (Html Msg)
viewAcuteIllnessPhysicalExam language currentDate site id isChw assembled data =
    let
        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectPhysicalExamTask currentDate person isChw assembled.initialEncounter) physicalExamTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        PhysicalExamVitals ->
                            ( "physical-exam-vitals"
                            , isJust measurements.vitals
                            )

                        PhysicalExamCoreExam ->
                            ( "physical-exam-core-exam"
                            , isJust measurements.coreExam
                            )

                        PhysicalExamMuac ->
                            ( "physical-exam-muac"
                            , isJust measurements.muac
                            )

                        PhysicalExamAcuteFindings ->
                            ( "acute-findings"
                            , isJust measurements.acuteFindings
                            )

                        PhysicalExamNutrition ->
                            ( "physical-exam-nutrition"
                            , isJust measurements.nutrition
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActivePhysicalExamTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PhysicalExamTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, physicalExamTasksCompletedFromTotal currentDate isChw person measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just PhysicalExamVitals ->
                    measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language
                            currentDate
                            isChw
                            assembled
                        |> List.singleton

                Just PhysicalExamCoreExam ->
                    measurements.coreExam
                        |> getMeasurementValueFunc
                        |> coreExamFormWithDefault data.coreExamForm
                        |> viewCoreExamForm language currentDate assembled
                        |> List.singleton

                Just PhysicalExamMuac ->
                    let
                        previousValue =
                            resolvePreviousValue assembled .muac muacValueFunc
                    in
                    measurements.muac
                        |> getMeasurementValueFunc
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language currentDate site assembled.person previousValue SetMuac

                Just PhysicalExamAcuteFindings ->
                    measurements.acuteFindings
                        |> getMeasurementValueFunc
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
                        |> viewAcuteFindingsForm language currentDate

                Just PhysicalExamNutrition ->
                    measurements.nutrition
                        |> getMeasurementValueFunc
                        |> Pages.AcuteIllness.Activity.Utils.nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate

                Nothing ->
                    []

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
                                    PhysicalExamVitals ->
                                        SaveVitals personId measurements.vitals nextTask

                                    PhysicalExamCoreExam ->
                                        SaveCoreExam personId measurements.coreExam nextTask

                                    PhysicalExamMuac ->
                                        SaveMuac personId measurements.muac nextTask

                                    PhysicalExamAcuteFindings ->
                                        SaveAcuteFindings personId measurements.acuteFindings nextTask

                                    PhysicalExamNutrition ->
                                        SaveNutrition personId measurements.nutrition nextTask
                        in
                        div [ class "actions symptoms" ]
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
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewVitalsForm : Language -> NominalDate -> Bool -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate isChw assembled form =
    let
        formConfig =
            { setIntInputMsg = SetVitalsIntInput
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .sys
            , diaBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .dia
            , heartRatePreviousValue =
                resolvePreviousMaybeValue assembled .vitals .heartRate
                    |> Maybe.map toFloat
            , respiratoryRatePreviousValue =
                resolvePreviousValue assembled .vitals .respiratoryRate
                    |> Maybe.map toFloat
            , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
            , birthDate = assembled.person.birthDate
            , formClass = "vitals"
            , mode =
                if isChw then
                    VitalsFormBasic

                else
                    VitalsFormFull
            , invokationModule = InvokationModuleAcuteIllness
            }
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewCoreExamForm : Language -> NominalDate -> AssembledData -> AcuteIllnessCoreExamForm -> Html Msg
viewCoreExamForm language currentDate assembled form =
    div [ class "ui form physical-exam core-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Heart ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.heart |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ NormalRateAndRhythm ]
                ]
            ]
        , viewCheckBoxSelectInput language
            [ IrregularRhythm, SinusTachycardia, NormalRateAndRhythm ]
            []
            form.heart
            SetCoreExamHeart
            Translate.HeartCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Lungs ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.lungs |> Maybe.withDefault [])
                    [ NormalLungs ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Wheezes, Crackles, NormalLungs ]
            []
            (form.lungs |> Maybe.withDefault [])
            Nothing
            SetCoreExamLungs
            Translate.LungsCPESign
        ]


viewAcuteFindingsForm : Language -> NominalDate -> AcuteFindingsForm -> List (Html Msg)
viewAcuteFindingsForm language currentDate form =
    [ div [ class "ui form physical-exam acute-findings" ]
        [ viewQuestionLabel language Translate.PatientExhibitAnyFindings
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ LethargicOrUnconscious, AcuteFindingsPoorSuck, SunkenEyes, PoorSkinTurgor, Jaundice, NoAcuteFindingsGeneralSigns ]
            []
            (form.signsGeneral |> Maybe.withDefault [])
            Nothing
            SetAcuteFindingsGeneralSign
            Translate.AcuteFindingsGeneralSign
        , viewQuestionLabel language Translate.PatientExhibitAnyRespiratoryFindings
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ Stridor, NasalFlaring, SevereWheezing, SubCostalRetractions, NoAcuteFindingsRespiratorySigns ]
            []
            (form.signsRespiratory |> Maybe.withDefault [])
            Nothing
            SetAcuteFindingsRespiratorySign
            Translate.AcuteFindingsRespiratorySign
        ]
    ]


viewNutritionForm : Language -> NominalDate -> AcuteIllnessNutritionForm -> List (Html Msg)
viewNutritionForm language currentDate form =
    [ div [ class "ui form physical-exam nutrition" ]
        [ p [] [ text <| translate language Translate.NutritionHelper ]
        , viewLabel language Translate.SelectAllSigns
        , viewCheckBoxMultipleSelectInput language
            [ Edema, AbdominalDistension, DrySkin ]
            [ Apathy, PoorAppetite, BrittleHair ]
            (form.signs |> Maybe.withDefault [])
            (Just NormalChildNutrition)
            SetNutritionSign
            Translate.ChildNutritionSignLabel
        ]
    ]


viewAcuteIllnessLaboratory :
    Language
    -> NominalDate
    -> AcuteIllnessEncounterId
    -> Bool
    -> AssembledData
    -> LaboratoryData
    -> List (Html Msg)
viewAcuteIllnessLaboratory language currentDate id isChw assembled data =
    let
        tasks =
            List.filter (expectLaboratoryTask currentDate isChw assembled) laboratoryTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        LaboratoryMalariaTesting ->
                            ( "laboratory-malaria-testing"
                            , isJust assembled.measurements.malariaTesting
                            )

                        LaboratoryCovidTesting ->
                            ( "laboratory-covid-testing"
                            , isJust assembled.measurements.covidTesting
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLaboratoryTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.AILaboratoryTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, laboratoryTasksCompletedFromTotal currentDate assembled.person assembled.measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        covidTestingForm =
            assembled.measurements.covidTesting
                |> getMeasurementValueFunc
                |> covidTestingFormWithDefault data.covidTestingForm

        viewForm =
            case activeTask of
                Just LaboratoryMalariaTesting ->
                    assembled.measurements.malariaTesting
                        |> getMeasurementValueFunc
                        |> malariaTestingFormWithDefault data.malariaTestingForm
                        |> viewMalariaTestingForm language currentDate assembled.person

                Just LaboratoryCovidTesting ->
                    viewCovidTestingForm language currentDate assembled.person covidTestingForm

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
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                LaboratoryMalariaTesting ->
                                    SaveMalariaTesting assembled.participant.person assembled.measurements.malariaTesting nextTask

                                LaboratoryCovidTesting ->
                                    let
                                        nextTask_ =
                                            case nextTask of
                                                Nothing ->
                                                    if covidTestingForm.testPerformed == Just False && feverRecorded assembled.measurements then
                                                        Just LaboratoryMalariaTesting

                                                    else
                                                        Nothing

                                                Just LaboratoryMalariaTesting ->
                                                    if covidTestingForm.testPerformed == Just True then
                                                        Nothing

                                                    else
                                                        Just LaboratoryMalariaTesting

                                                _ ->
                                                    Nothing
                                    in
                                    SaveCovidTesting assembled.participant.person assembled.measurements.covidTesting nextTask_
                    in
                    div [ class "actions malaria-testing" ]
                        [ button
                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                            , onClick saveMsg
                            ]
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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewMalariaTestingForm : Language -> NominalDate -> Person -> MalariaTestingForm -> Html Msg
viewMalariaTestingForm language currentDate person form =
    let
        resultInput =
            viewCustomSelectListInput form.rapidTestResult
                [ RapidTestNegative, RapidTestPositive, RapidTestIndeterminate, RapidTestUnableToRun ]
                malariaRapidTestResultAsString
                SetRapidTestResult
                (Translate.RapidTestResult >> translate language)
                "form-input rapid-test-result"
                (isNothing form.rapidTestResult)

        testResultPositive =
            form.rapidTestResult == Just RapidTestPositive || form.rapidTestResult == Just RapidTestPositiveAndPregnant

        isPregnantInput =
            if testResultPositive && isPersonAFertileWoman currentDate person then
                viewIsPregnantInput language SetIsPregnant form.isPregnant

            else
                []
    in
    div [ class "ui form laboratory malaria-testing" ] <|
        [ viewLabel language Translate.MalariaRapidDiagnosticTest
        , resultInput
        ]
            ++ isPregnantInput


viewCovidTestingForm : Language -> NominalDate -> Person -> CovidTestingForm -> Html Msg
viewCovidTestingForm language currentDate person form =
    let
        isPregnantInputForView =
            if isPersonAFertileWoman currentDate person then
                viewIsPregnantInput language (SetCovidTestingBoolInput (\value form_ -> { form_ | isPregnant = Just value })) form.isPregnant

            else
                []

        derivedInputs =
            Maybe.map
                (\testPerformed ->
                    if testPerformed then
                        let
                            isPregnantInput =
                                if form.testPositive == Just True then
                                    isPregnantInputForView

                                else
                                    []
                        in
                        [ viewQuestionLabel language Translate.TestResultsQuestion
                        , viewBoolInput
                            language
                            form.testPositive
                            (SetCovidTestingBoolInput (\value form_ -> { form_ | testPositive = Just value, isPregnant = Nothing }))
                            "test-result"
                            (Just ( Translate.RapidTestResult RapidTestPositive, Translate.RapidTestResult RapidTestNegative ))
                        ]
                            ++ isPregnantInput

                    else
                        div [ class "why-not" ]
                            [ viewQuestionLabel language Translate.WhyNot
                            , viewCheckBoxSelectInput language
                                [ AdministeredPreviously
                                , NonAdministrationLackOfStock
                                , NonAdministrationPatientDeclined
                                , NonAdministrationPatientUnableToAfford
                                , NonAdministrationOther
                                ]
                                []
                                form.administrationNote
                                SetCovidTestingAdministrationNote
                                Translate.AdministrationNote
                            ]
                            :: isPregnantInputForView
                )
                form.testPerformed
                |> Maybe.withDefault []
    in
    div [ class "ui form laboratory covid-testing" ] <|
        [ viewCustomLabel language Translate.CovidTestingInstructions "." "instructions"
        , viewQuestionLabel language Translate.TestPerformedQuestion
        , viewBoolInput
            language
            form.testPerformed
            (SetCovidTestingBoolInput
                (\value form_ ->
                    { form_
                        | testPerformed = Just value
                        , testPositive = Nothing
                        , isPregnant = Nothing
                        , administrationNote = Nothing
                    }
                )
            )
            "test-performed"
            Nothing
        ]
            ++ derivedInputs


viewIsPregnantInput : Language -> (Bool -> Msg) -> Maybe Bool -> List (Html Msg)
viewIsPregnantInput language setMsg currentValue =
    [ viewQuestionLabel language Translate.CurrentlyPregnantQuestion
    , viewBoolInput
        language
        currentValue
        setMsg
        "is-pregnant"
        Nothing
    ]


viewAcuteIllnessExposure : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> ExposureData -> List (Html Msg)
viewAcuteIllnessExposure language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ ExposureTravel, ExposureExposure ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        ExposureTravel ->
                            ( "exposure-travel"
                            , isJust measurements.travelHistory
                            )

                        ExposureExposure ->
                            ( "exposure-exposure"
                            , isJust measurements.exposure
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveExposureTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ExposureTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, exposureTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                ExposureTravel ->
                    measurements.travelHistory
                        |> getMeasurementValueFunc
                        |> travelHistoryFormWithDefault data.travelHistoryForm
                        |> viewTravelHistoryForm language currentDate measurements

                ExposureExposure ->
                    measurements.exposure
                        |> getMeasurementValueFunc
                        |> exposureFormWithDefault data.exposureForm
                        |> viewExposureForm language currentDate measurements

        getNextTask currentTask =
            case data.activeTask of
                ExposureTravel ->
                    [ ExposureExposure ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                ExposureExposure ->
                    [ ExposureTravel ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveMsg =
                    case data.activeTask of
                        ExposureTravel ->
                            SaveTravelHistory personId measurements.travelHistory nextTask

                        ExposureExposure ->
                            SaveExposure personId measurements.exposure nextTask
            in
            div [ class "actions exposure" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveMsg
                    ]
                    [ text <| translate language Translate.Save ]
                ]
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
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


viewTravelHistoryForm : Language -> NominalDate -> AcuteIllnessMeasurements -> TravelHistoryForm -> Html Msg
viewTravelHistoryForm language currentDate measurements form =
    div [ class "ui form exposure travel-history" ]
        [ viewQuestionLabel language Translate.TraveledToCOVID19CountryQuestion
        , viewBoolInput
            language
            form.covid19Country
            SetCovid19Country
            "covid19-country"
            Nothing
        ]


viewExposureForm : Language -> NominalDate -> AcuteIllnessMeasurements -> ExposureForm -> Html Msg
viewExposureForm language currentDate measurements form =
    div [ class "ui form exposure" ]
        [ viewQuestionLabel language Translate.ContactWithCOVID19SymptomsQuestion
        , div [ class "question-helper" ] [ text <| translate language Translate.ContactWithCOVID19SymptomsHelper ++ "." ]
        , viewBoolInput
            language
            form.covid19Symptoms
            SetCovid19Symptoms
            "covid19-symptoms"
            Nothing
        ]


viewHCRecommendation : Language -> HCRecommendation -> Html any
viewHCRecommendation language recommendation =
    let
        riskLevel =
            case recommendation of
                SendAmbulance ->
                    Translate.HighRiskCase

                HomeIsolation ->
                    Translate.HighRiskCase

                ComeToHealthCenter ->
                    Translate.LowRiskCase

                ChwMonitoring ->
                    Translate.LowRiskCase

                HCRecommendationNotApplicable ->
                    Translate.LowRiskCase
    in
    label []
        [ text <| translate language Translate.HealthCenterDetermined
        , span [ class "strong" ] [ text <| translate language riskLevel ]
        , text <| translate language Translate.AndSentence
        , span [ class "strong" ] [ text <| translate language <| Translate.HCRecommendation recommendation ]
        ]


viewAcuteIllnessPriorTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> PriorTreatmentData -> List (Html Msg)
viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ TreatmentReview ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TreatmentReview ->
                            ( "treatment-review"
                            , isJust measurements.treatmentReview
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActivePriorTreatmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PriorTreatmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, treatmentTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                TreatmentReview ->
                    measurements.treatmentReview
                        |> getMeasurementValueFunc
                        |> treatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewTreatmentReviewForm language currentDate measurements

        actions =
            let
                saveMsg =
                    case data.activeTask of
                        TreatmentReview ->
                            SaveTreatmentReview personId measurements.treatmentReview
            in
            div [ class "actions malaria-testing" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveMsg
                    ]
                    [ text <| translate language Translate.Save ]
                ]
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


viewTreatmentReviewForm : Language -> NominalDate -> AcuteIllnessMeasurements -> TreatmentReviewForm -> Html Msg
viewTreatmentReviewForm language currentDate measurements form =
    let
        feverPast6HoursUpdateFunc value form_ =
            if value then
                { form_ | feverPast6Hours = Just True }

            else
                { form_ | feverPast6Hours = Just False, feverPast6HoursHelped = Nothing }

        feverPast6HoursHelpedUpdateFunc value form_ =
            { form_ | feverPast6HoursHelped = Just value }

        malariaTodayUpdateFunc value form_ =
            if value then
                { form_ | malariaToday = Just True }

            else
                { form_ | malariaToday = Just False, malariaTodayHelped = Nothing }

        malariaTodayHelpedUpdateFunc value form_ =
            { form_ | malariaTodayHelped = Just value }

        malariaWithinPastMonthUpdateFunc value form_ =
            if value then
                { form_ | malariaWithinPastMonth = Just True }

            else
                { form_ | malariaWithinPastMonth = Just False, malariaWithinPastMonthHelped = Nothing }

        malariaWithinPastMonthHelpedUpdateFunc value form_ =
            { form_ | malariaWithinPastMonthHelped = Just value }

        medicationHelpedQuestion =
            div [ class "ui grid" ]
                [ div [ class "one wide column" ] []
                , div [ class "fifteen wide column" ]
                    [ viewQuestionLabel language Translate.MedicationHelpedQuestion ]
                ]

        feverPast6HoursSection =
            let
                feverPast6HoursPositive =
                    form.feverPast6Hours
                        |> Maybe.withDefault False

                feverPast6HoursHelpedInput =
                    if feverPast6HoursPositive then
                        [ medicationHelpedQuestion
                        , viewBoolInput
                            language
                            form.feverPast6HoursHelped
                            (SetTreatmentReviewBoolInput feverPast6HoursHelpedUpdateFunc)
                            "fever-past-6-hours-helped derived"
                            Nothing
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationForFeverPast6HoursQuestion
            , viewBoolInput
                language
                form.feverPast6Hours
                (SetTreatmentReviewBoolInput feverPast6HoursUpdateFunc)
                "fever-past-6-hours"
                Nothing
            ]
                ++ feverPast6HoursHelpedInput

        malariaTodaySection =
            let
                malariaTodayPositive =
                    form.malariaToday
                        |> Maybe.withDefault False

                malariaTodayHelpedInput =
                    if malariaTodayPositive then
                        [ medicationHelpedQuestion
                        , viewBoolInput
                            language
                            form.malariaTodayHelped
                            (SetTreatmentReviewBoolInput malariaTodayHelpedUpdateFunc)
                            "malaria-today-helped derived"
                            Nothing
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationForMalariaTodayQuestion
            , viewBoolInput
                language
                form.malariaToday
                (SetTreatmentReviewBoolInput malariaTodayUpdateFunc)
                "malaria-today"
                Nothing
            ]
                ++ malariaTodayHelpedInput

        malariaWithinPastMonthSection =
            let
                malariaWithinPastMonthPositive =
                    form.malariaWithinPastMonth
                        |> Maybe.withDefault False

                malariaWithinPastMonthHelpedInput =
                    if malariaWithinPastMonthPositive then
                        [ medicationHelpedQuestion
                        , viewBoolInput
                            language
                            form.malariaWithinPastMonthHelped
                            (SetTreatmentReviewBoolInput malariaWithinPastMonthHelpedUpdateFunc)
                            "malaria-within-past-month-helped derived"
                            Nothing
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationForMalariaWithinPastMonthQuestion
            , viewBoolInput
                language
                form.malariaWithinPastMonth
                (SetTreatmentReviewBoolInput malariaWithinPastMonthUpdateFunc)
                "malaria-within-past-month"
                Nothing
            ]
                ++ malariaWithinPastMonthHelpedInput
    in
    feverPast6HoursSection
        ++ malariaTodaySection
        ++ malariaWithinPastMonthSection
        |> div [ class "ui form treatment-review" ]


viewAcuteIllnessNextSteps : Language -> NominalDate -> Site -> GeoInfo -> AcuteIllnessEncounterId -> Bool -> AssembledData -> ModelIndexedDb -> NextStepsData -> List (Html Msg)
viewAcuteIllnessNextSteps language currentDate site geoInfo id isChw assembled db data =
    let
        person =
            assembled.person

        measurements =
            assembled.measurements

        diagnosis =
            Maybe.map Tuple.second assembled.diagnosis

        tasks =
            resolveNextStepsTasks currentDate isChw assembled

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        NextStepsIsolation ->
                            ( "next-steps-isolation"
                            , isJust measurements.isolation
                            )

                        NextStepsContactHC ->
                            ( "next-steps-call"
                            , isJust measurements.hcContact
                            )

                        NextStepsCall114 ->
                            ( "next-steps-call"
                            , isJust measurements.call114
                            )

                        NextStepsMedicationDistribution ->
                            ( "next-steps-medication-distribution"
                            , isJust measurements.medicationDistribution
                            )

                        NextStepsSendToHC ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                        NextStepsHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        NextStepsFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                        NextStepsContactTracing ->
                            ( "next-steps-contacts-tracing"
                            , isJust measurements.contactsTracing
                            )

                        NextStepsSymptomsReliefGuidance ->
                            -- We call it Relief Guidance, but
                            -- make use of Health Education form.
                            ( "next-steps-medication-distribution"
                            , isJust measurements.healthEducation
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
                    , text <| translate language (Translate.NextStepsTask isChw task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal isChw assembled.initialEncounter diagnosis measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        call114Form =
            measurements.call114
                |> getMeasurementValueFunc
                |> call114FormWithDefault data.call114Form

        viewForm =
            case activeTask of
                Just NextStepsIsolation ->
                    measurements.isolation
                        |> getMeasurementValueFunc
                        |> isolationFormWithDefault data.isolationForm
                        |> viewIsolationForm language currentDate isChw measurements

                Just NextStepsContactHC ->
                    measurements.hcContact
                        |> getMeasurementValueFunc
                        |> hcContactFormWithDefault data.hcContactForm
                        |> viewHCContactForm language currentDate assembled.initialEncounter measurements

                Just NextStepsCall114 ->
                    viewCall114Form language currentDate measurements call114Form

                Just NextStepsMedicationDistribution ->
                    measurements.medicationDistribution
                        |> getMeasurementValueFunc
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> viewMedicationDistributionForm language currentDate person diagnosis

                Just NextStepsSendToHC ->
                    let
                        sendToFacilityFunc =
                            if isChw then
                                viewSendToHealthCenterForm

                            else
                                viewSendToHospitalForm
                    in
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityFunc language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing

                Just NextStepsHealthEducation ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate diagnosis

                Just NextStepsFollowUp ->
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate isChw

                Just NextStepsContactTracing ->
                    measurements.contactsTracing
                        |> getMeasurementValueFunc
                        |> contactsTracingFormWithDefault data.contactsTracingForm
                        |> viewContactsTracingForm language currentDate site geoInfo db contactsTracingFinished

                Just NextStepsSymptomsReliefGuidance ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewSymptomsReliefForm language currentDate diagnosis

                Nothing ->
                    emptyNode

        tasksAfterSave =
            case activeTask of
                -- On first visit, ContactHC task should appear in case nurse did not talk to 114.
                -- Therefore, when the answer to 'called 114' is changed, we adjust tasks list accordingly.
                Just NextStepsCall114 ->
                    if assembled.initialEncounter then
                        if call114Form.called114 == Just False then
                            [ NextStepsIsolation, NextStepsCall114, NextStepsContactHC, NextStepsFollowUp ]

                        else if call114Form.called114 == Just True then
                            [ NextStepsIsolation, NextStepsCall114, NextStepsFollowUp ]

                        else
                            tasks

                    else
                        tasks

                -- At subsequent visit, SendToHC task should appear in case health center adviced to send patient over.
                -- Therefore, when the answer to this is changed, we adjust tasks list accirdingly.
                Just NextStepsContactHC ->
                    if assembled.initialEncounter then
                        tasks

                    else
                        let
                            hcContactForm =
                                measurements.hcContact
                                    |> getMeasurementValueFunc
                                    |> hcContactFormWithDefault data.hcContactForm
                        in
                        if healthCenterRecommendedToCome measurements && hcContactForm.recommendations /= Just ComeToHealthCenter then
                            [ NextStepsContactHC, NextStepsHealthEducation, NextStepsFollowUp ]

                        else if (not <| healthCenterRecommendedToCome measurements) && hcContactForm.recommendations == Just ComeToHealthCenter then
                            [ NextStepsContactHC, NextStepsSendToHC, NextStepsHealthEducation, NextStepsFollowUp ]

                        else
                            tasks

                -- If medication is prescribed, but it's out of stock, or partient
                -- if alergic, SendToHC should appear, so that patient is dircted to the HC.
                -- An exclusion here is when patient is diagnosed with Covid and Pneumonia,
                -- where the patient is monitored at home.
                Just NextStepsMedicationDistribution ->
                    let
                        medicationDistributionForm =
                            measurements.medicationDistribution
                                |> getMeasurementValueFunc
                                |> medicationDistributionFormWithDefault data.medicationDistributionForm

                        medicationOutOfStockOrPatientAlergic =
                            medicationDistributionForm.nonAdministrationSigns
                                |> Maybe.map
                                    (\signs ->
                                        [ MedicationAmoxicillin NonAdministrationLackOfStock
                                        , MedicationAmoxicillin NonAdministrationKnownAllergy
                                        , MedicationCoartem NonAdministrationLackOfStock
                                        , MedicationCoartem NonAdministrationKnownAllergy
                                        , MedicationORS NonAdministrationLackOfStock
                                        , MedicationORS NonAdministrationKnownAllergy
                                        , MedicationZinc NonAdministrationLackOfStock
                                        , MedicationZinc NonAdministrationKnownAllergy
                                        ]
                                            |> List.any (\option -> EverySet.member option signs)
                                    )
                                |> Maybe.withDefault False
                    in
                    if List.member diagnosis [ Just DiagnosisPneuminialCovid19, Just DiagnosisLowRiskCovid19 ] then
                        tasks

                    else if medicationOutOfStockOrPatientAlergic then
                        [ NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsFollowUp ]

                    else
                        [ NextStepsMedicationDistribution, NextStepsFollowUp ]

                _ ->
                    tasks

        nextTask =
            tasksAfterSave
                |> List.filter
                    (\task ->
                        (Just task /= activeTask)
                            && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                    )
                |> List.head

        contactsTracingFinished =
            isJust assembled.measurements.contactsTracing || data.contactsTracingForm.finished

        actions =
            Maybe.map
                (\task ->
                    if task == NextStepsContactTracing && not contactsTracingFinished then
                        emptyNode

                    else
                        let
                            personId =
                                assembled.participant.person

                            saveMsg =
                                case task of
                                    NextStepsIsolation ->
                                        SaveIsolation personId measurements.isolation nextTask

                                    NextStepsContactHC ->
                                        SaveHCContact personId measurements.hcContact nextTask

                                    NextStepsCall114 ->
                                        SaveCall114 personId measurements.call114 nextTask

                                    NextStepsSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                                    NextStepsMedicationDistribution ->
                                        SaveMedicationDistribution personId measurements.medicationDistribution nextTask

                                    NextStepsHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    NextStepsFollowUp ->
                                        SaveFollowUp personId
                                            (Maybe.map Tuple.second assembled.diagnosis)
                                            measurements.followUp
                                            nextTask

                                    NextStepsContactTracing ->
                                        SaveContactsTracing personId measurements.contactsTracing nextTask

                                    NextStepsSymptomsReliefGuidance ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                            saveLabel =
                                case task of
                                    NextStepsHealthEducation ->
                                        if noImprovementOnSubsequentVisit currentDate person measurements then
                                            Translate.Save

                                        else
                                            Translate.SaveAndRecordOutcome

                                    _ ->
                                        Translate.Save

                            disabled =
                                case task of
                                    NextStepsContactTracing ->
                                        -- After traceing is finished and saved, we
                                        -- do not allow additional editing.
                                        isJust assembled.measurements.contactsTracing

                                    _ ->
                                        tasksCompleted /= totalTasks
                        in
                        div [ class "actions next-steps" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                                , onClick saveMsg
                                ]
                                [ text <| translate language saveLabel ]
                            ]
                )
                activeTask
                |> Maybe.withDefault emptyNode

        fullScreen =
            Maybe.map ((==) NextStepsContactTracing) activeTask
                |> Maybe.withDefault False
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui three column grid" ] <|
            List.map viewTask tasks
        ]
    , div
        [ classList
            [ ( "tasks-count", True )
            , ( "full-screen", fullScreen )
            ]
        ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewIsolationForm : Language -> NominalDate -> Bool -> AcuteIllnessMeasurements -> IsolationForm -> Html Msg
viewIsolationForm language currentDate isChw measurements form =
    let
        headerHelper =
            if isChw then
                emptyNode

            else
                viewCustomLabel language Translate.AcuteIllnessLowRiskCaseHelper "." "instructions"

        patientIsolatedInput =
            [ viewQuestionLabel language (Translate.PatientIsolatedQuestion isChw)
            , viewBoolInput
                language
                form.patientIsolated
                SetPatientIsolated
                "patient-isolated"
                Nothing
            ]

        derivedInputs =
            case form.patientIsolated of
                Just True ->
                    let
                        signOnDoorInput =
                            if isChw then
                                [ viewQuestionLabel language Translate.SignOnDoorPostedQuestion
                                , viewBoolInput
                                    language
                                    form.signOnDoor
                                    SetSignOnDoor
                                    "sign-on-door"
                                    Nothing
                                ]

                            else
                                []
                    in
                    signOnDoorInput ++ healthEducationInput

                Just False ->
                    [ viewQuestionLabel language Translate.WhyNot
                    , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
                    , viewCheckBoxMultipleSelectInput language
                        [ NoSpace, TooIll, CanNotSeparateFromFamily, OtherReason ]
                        []
                        (form.reasonsForNotIsolating |> Maybe.withDefault [])
                        Nothing
                        SetReasonForNotIsolating
                        Translate.ReasonForNotIsolating
                    ]
                        ++ healthEducationInput

                Nothing ->
                    []

        healthEducationInput =
            [ viewQuestionLabel language Translate.HealthEducationProvidedQuestion
            , viewBoolInput
                language
                form.healthEducation
                SetHealthEducation
                "health-education"
                Nothing
            ]
    in
    (headerHelper :: patientIsolatedInput)
        ++ derivedInputs
        |> div [ class "ui form next-steps isolation" ]


viewHCContactForm : Language -> NominalDate -> Bool -> AcuteIllnessMeasurements -> HCContactForm -> Html Msg
viewHCContactForm language currentDate initialEncounter measurements form =
    let
        contactedHCInput =
            [ viewQuestionLabel language Translate.ContactedHCQuestion
            , viewBoolInput
                language
                form.contactedHC
                SetContactedHC
                "contacted-hc"
                Nothing
            ]

        derivedInputs =
            case form.contactedHC of
                Just True ->
                    let
                        hcRespnonseOptions =
                            if initialEncounter then
                                [ SendAmbulance, HomeIsolation, ComeToHealthCenter, ChwMonitoring ]

                            else
                                [ SendAmbulance, ComeToHealthCenter ]

                        hcRespnonseInput =
                            [ viewQuestionLabel language Translate.HCResponseQuestion
                            , viewCheckBoxSelectCustomInput language
                                hcRespnonseOptions
                                []
                                form.recommendations
                                SetHCRecommendation
                                (viewHCRecommendation language)
                            ]

                        hcRespnonsePeriodInput =
                            [ viewQuestionLabel language Translate.HCResponsePeriodQuestion
                            , viewCheckBoxSelectInput language
                                [ LessThan30Min, Between30min1Hour, Between1Hour2Hour, Between2Hour1Day ]
                                []
                                form.responsePeriod
                                SetResponsePeriod
                                Translate.ResponsePeriod
                            ]

                        derivedInput =
                            form.recommendations
                                |> Maybe.map
                                    (\recommendations ->
                                        if recommendations == SendAmbulance then
                                            [ viewQuestionLabel language Translate.AmbulancArrivalPeriodQuestion
                                            , viewCheckBoxSelectInput language
                                                [ LessThan30Min, Between30min1Hour, Between1Hour2Hour, Between2Hour1Day ]
                                                []
                                                form.ambulanceArrivalPeriod
                                                SetAmbulanceArrivalPeriod
                                                Translate.ResponsePeriod
                                            ]

                                        else
                                            []
                                    )
                                |> Maybe.withDefault []
                    in
                    hcRespnonseInput ++ hcRespnonsePeriodInput ++ derivedInput

                _ ->
                    []
    in
    contactedHCInput
        ++ derivedInputs
        |> div [ class "ui form exposure hc-contact" ]


viewCall114Form : Language -> NominalDate -> AcuteIllnessMeasurements -> Call114Form -> Html Msg
viewCall114Form language currentDate measurements form =
    let
        header =
            [ viewCustomLabel language Translate.Call114 "" "helper call-114"
            , div
                [ class "review-case-wrapper"
                , onClick <| SetPertinentSymptomsPopupState True
                ]
                [ viewCustomLabel language Translate.ReviewCaseWith144Respondent "" "helper review-case"
                , img [ src "assets/images/icon-review.png" ] []
                ]
            ]

        called114Input =
            [ viewQuestionLabel language Translate.Called114Question
            , viewBoolInput
                language
                form.called114
                SetCalled114
                "called-114"
                Nothing
            ]

        derivedInputs =
            form.called114
                |> Maybe.map
                    (\called114 ->
                        if called114 then
                            let
                                recommendation114Input =
                                    [ viewQuestionLabel language Translate.WhatWasTheirResponse
                                    , viewCheckBoxSelectInput language
                                        [ SendToHealthCenter, SendToRRTCenter, SendToHospital, OtherRecommendation114 ]
                                        []
                                        form.recommendation114
                                        SetRecommendation114
                                        Translate.Recommendation114
                                    ]

                                derivedSiteInputs =
                                    if isJust form.recommendation114 && form.recommendation114 /= Just OtherRecommendation114 then
                                        let
                                            contactedSiteInput =
                                                [ viewQuestionLabel language Translate.ContactedRecommendedSiteQuestion
                                                , viewBoolInput
                                                    language
                                                    form.contactedSite
                                                    SetContactedSite
                                                    "contacted-site"
                                                    Nothing
                                                ]

                                            recommndationSiteInput =
                                                form.contactedSite
                                                    |> Maybe.map
                                                        (\contactedSite ->
                                                            if contactedSite then
                                                                [ viewQuestionLabel language Translate.WhatWasTheirResponse
                                                                , viewCheckBoxSelectInput language
                                                                    [ TeamComeToVillage, SendToSiteWithForm, OtherRecommendationSite ]
                                                                    []
                                                                    form.recommendationSite
                                                                    SetRecommendationSite
                                                                    Translate.RecommendationSite
                                                                ]

                                                            else
                                                                [ viewQuestionLabel language Translate.WhyNot
                                                                , viewCheckBoxSelectInput language
                                                                    [ NoneSentWithForm, NonePatientRefused, NoneOtherRecommendationSite ]
                                                                    []
                                                                    form.recommendationSite
                                                                    SetRecommendationSite
                                                                    Translate.RecommendationSite
                                                                ]
                                                        )
                                                    |> Maybe.withDefault []
                                        in
                                        contactedSiteInput ++ recommndationSiteInput

                                    else
                                        []
                            in
                            recommendation114Input ++ derivedSiteInputs

                        else
                            [ viewQuestionLabel language Translate.WhyNot
                            , viewCheckBoxSelectInput language
                                [ NoneNoAnswer, NoneBusySignal, NoneOtherRecommendation114 ]
                                []
                                form.recommendation114
                                SetRecommendation114
                                Translate.Recommendation114
                            ]
                    )
                |> Maybe.withDefault []
    in
    header
        ++ called114Input
        ++ derivedInputs
        |> div [ class "ui form next-steps call-114" ]


viewMedicationDistributionForm : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> MedicationDistributionForm -> Html Msg
viewMedicationDistributionForm language currentDate person diagnosis form =
    let
        ( instructions, questions ) =
            let
                viewDerivedQuestion medication reasonToSignFunc =
                    let
                        currentValue =
                            getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
                    in
                    [ viewQuestionLabel language Translate.WhyNot
                    , viewCheckBoxSelectInput language
                        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
                        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                        currentValue
                        (SetMedicationDistributionAdministrationNote currentValue medication)
                        Translate.AdministrationNote
                    ]

                -- When answer for medication administartion is Yes, we clean the reason for not adminsetering the medication.
                updateNonAdministrationSigns medication reasonToSignFunc value form_ =
                    if value then
                        form_.nonAdministrationSigns
                            |> Maybe.andThen
                                (\nonAdministrationSigns ->
                                    getCurrentReasonForMedicationNonAdministration reasonToSignFunc form_
                                        |> Maybe.map
                                            (\reason ->
                                                Just <| EverySet.remove (nonAdministrationReasonToSign medication reason) nonAdministrationSigns
                                            )
                                        |> Maybe.withDefault (Just nonAdministrationSigns)
                                )

                    else
                        form_.nonAdministrationSigns

                amoxicillinAdministration =
                    resolveAmoxicillinDosage currentDate person
                        |> Maybe.map
                            (\( numberOfPills, pillMass, duration ) ->
                                let
                                    amoxicillinUpdateFunc value form_ =
                                        { form_ | amoxicillin = Just value, nonAdministrationSigns = updateNonAdministrationSigns Amoxicillin MedicationAmoxicillin value form_ }

                                    derivedQuestion =
                                        case form.amoxicillin of
                                            Just False ->
                                                viewDerivedQuestion Amoxicillin MedicationAmoxicillin

                                            _ ->
                                                []

                                    administeredMedicationQuestion =
                                        if pillMass == "500" then
                                            viewQuestionLabel language Translate.AdministeredOneOfAboveMedicinesQuestion

                                        else
                                            viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Amoxicillin)
                                in
                                ( div [ class "instructions respiratory-infection-uncomplicated" ] <|
                                    viewAmoxicillinAdministrationInstructions language numberOfPills pillMass duration Nothing
                                , [ administeredMedicationQuestion
                                  , viewBoolInput
                                        language
                                        form.amoxicillin
                                        (SetMedicationDistributionBoolInput amoxicillinUpdateFunc)
                                        "amoxicillin-medication"
                                        Nothing
                                  ]
                                    ++ derivedQuestion
                                )
                            )
                        |> Maybe.withDefault ( emptyNode, [] )
            in
            case diagnosis of
                Just DiagnosisMalariaUncomplicated ->
                    let
                        coartemUpdateFunc value form_ =
                            { form_ | coartem = Just value, nonAdministrationSigns = updateNonAdministrationSigns Coartem MedicationCoartem value form_ }

                        derivedQuestion =
                            case form.coartem of
                                Just False ->
                                    viewDerivedQuestion Coartem MedicationCoartem

                                _ ->
                                    []
                    in
                    ( resolveCoartemDosage currentDate person
                        |> Maybe.map
                            (\dosage ->
                                div [ class "instructions malaria-uncomplicated" ]
                                    [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign Coartem) "icon-pills" Nothing
                                    , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 3)
                                    ]
                            )
                        |> Maybe.withDefault emptyNode
                    , [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Coartem)
                      , viewBoolInput
                            language
                            form.coartem
                            (SetMedicationDistributionBoolInput coartemUpdateFunc)
                            "coartem-medication"
                            Nothing
                      ]
                        ++ derivedQuestion
                    )

                Just DiagnosisGastrointestinalInfectionUncomplicated ->
                    let
                        orsUpdateFunc value form_ =
                            { form_ | ors = Just value, nonAdministrationSigns = updateNonAdministrationSigns ORS MedicationORS value form_ }

                        zincUpdateFunc value form_ =
                            { form_ | zinc = Just value, nonAdministrationSigns = updateNonAdministrationSigns Zinc MedicationZinc value form_ }

                        orsDerivedQuestion =
                            case form.ors of
                                Just False ->
                                    viewDerivedQuestion ORS MedicationORS

                                _ ->
                                    []

                        zincDerivedQuestion =
                            case form.zinc of
                                Just False ->
                                    viewDerivedQuestion Zinc MedicationZinc

                                _ ->
                                    []
                    in
                    ( Maybe.map2
                        (\orsDosage zincDosage ->
                            div [ class "instructions gastrointestinal-uncomplicated" ]
                                [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign ORS) "icon-oral-solution" Nothing
                                , viewOralSolutionPrescription language orsDosage
                                , viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign Zinc) "icon-pills" Nothing
                                , viewTabletsPrescription language zincDosage (Translate.ByMouthDaylyForXDays 10)
                                ]
                        )
                        (resolveORSDosage currentDate person)
                        (resolveZincDosage currentDate person)
                        |> Maybe.withDefault emptyNode
                    , [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign ORS)
                      , viewBoolInput
                            language
                            form.ors
                            (SetMedicationDistributionBoolInput orsUpdateFunc)
                            "ors-medication"
                            Nothing
                      ]
                        ++ orsDerivedQuestion
                        ++ [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Zinc)
                           , viewBoolInput
                                language
                                form.zinc
                                (SetMedicationDistributionBoolInput zincUpdateFunc)
                                "zinc-medication"
                                Nothing
                           ]
                        ++ zincDerivedQuestion
                    )

                Just DiagnosisSimpleColdAndCough ->
                    let
                        lemonJuiceOrHoneyUpdateFunc value form_ =
                            { form_ | lemonJuiceOrHoney = Just value }
                    in
                    ( div [ class "instructions simple-cough-and-cold" ]
                        [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign LemonJuiceOrHoney) "icon-pills" Nothing ]
                    , [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign LemonJuiceOrHoney)
                      , viewBoolInput
                            language
                            form.lemonJuiceOrHoney
                            (SetMedicationDistributionBoolInput lemonJuiceOrHoneyUpdateFunc)
                            "lemon-juice-or-honey-medication"
                            Nothing
                      ]
                    )

                Just DiagnosisRespiratoryInfectionUncomplicated ->
                    amoxicillinAdministration

                Just DiagnosisPneuminialCovid19 ->
                    amoxicillinAdministration

                _ ->
                    ( emptyNode, [] )
    in
    div [ class "ui form medication-distribution" ] <|
        [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
        , instructions
        ]
            ++ questions


viewAmoxicillinAdministrationInstructions : Language -> String -> String -> TranslationId -> Maybe NominalDate -> List (Html any)
viewAmoxicillinAdministrationInstructions language numberOfPills pillMassInMg duration maybeDate =
    let
        ( medicationLabelSuffix, prescription ) =
            if numberOfPills == "0.5" then
                ( " (" ++ (translate language <| Translate.HalfOfDosage pillMassInMg) ++ ")"
                , div [ class "prescription" ]
                    [ text <| translate language Translate.SeeDosageScheduleByWeight ]
                )

            else
                ( " (" ++ pillMassInMg ++ ")"
                , viewTabletsPrescription language numberOfPills duration
                )

        alternateMedicineSection =
            if pillMassInMg == "500" then
                [ p [ class "or" ] [ text <| translate language Translate.Or ]
                , viewAdministeredMedicationCustomLabel
                    language
                    Translate.Administer
                    Translate.MedicationDoxycycline
                    ""
                    "icon-pills"
                    ":"
                    maybeDate
                , viewTabletsPrescription language "1" (Translate.ByMouthTwiceADayForXDays 5)
                ]

            else
                []
    in
    [ viewAdministeredMedicationCustomLabel
        language
        Translate.Administer
        (Translate.MedicationDistributionSign Amoxicillin)
        medicationLabelSuffix
        "icon-pills"
        ":"
        maybeDate
    , prescription
    ]
        ++ alternateMedicineSection


viewParacetamolAdministrationInstructions : Language -> Maybe NominalDate -> Bool -> List (Html any)
viewParacetamolAdministrationInstructions language maybeDate isAdult =
    let
        ( medicationLabelSuffix, prescription ) =
            if isAdult then
                ( " (1g)", Translate.ParacetamolPrescriptionForAdult )

            else
                ( " (15mg per kg)", Translate.SeeDosageScheduleByWeight )
    in
    [ viewAdministeredMedicationCustomLabel
        language
        Translate.Administer
        (Translate.MedicationDistributionSign Paracetamol)
        medicationLabelSuffix
        "icon-pills"
        ":"
        maybeDate
    , div [ class "prescription" ]
        [ text <| translate language prescription ]
    ]


viewAdministeredMedicationQuestion : Language -> TranslationId -> Html any
viewAdministeredMedicationQuestion language medicineTranslationId =
    div [ class "label" ]
        [ text <|
            translate language Translate.AdministeredMedicationQuestion
                ++ " "
                ++ translate language medicineTranslationId
                ++ " "
                ++ translate language Translate.ToThePatient
                ++ "?"
        ]


viewAdministeredMedicationLabel : Language -> TranslationId -> TranslationId -> String -> Maybe NominalDate -> Html any
viewAdministeredMedicationLabel language administerTranslationId medicineTranslationId iconClass maybeDate =
    viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId "" iconClass ":" maybeDate


viewAdministeredMedicationCustomLabel : Language -> TranslationId -> TranslationId -> String -> String -> String -> Maybe NominalDate -> Html any
viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId medicineSuffix iconClass suffix maybeDate =
    let
        message =
            div [] <|
                [ text <| translate language administerTranslationId
                , text ": "
                , span [ class "medicine" ] [ text <| translate language medicineTranslationId ++ medicineSuffix ]
                ]
                    ++ renderDatePart language maybeDate
                    ++ [ text <| " " ++ suffix ]
    in
    viewInstructionsLabel iconClass message


viewTabletsPrescription : Language -> String -> TranslationId -> Html any
viewTabletsPrescription language dosage duration =
    div [ class "prescription" ]
        [ span [] [ text <| translate language (Translate.TabletSinglePlural dosage) ]
        , text " "
        , text <| translate language duration
        , text "."
        ]


viewOralSolutionPrescription : Language -> String -> Html any
viewOralSolutionPrescription language dosage =
    div [ class "prescription" ]
        [ span [] [ text <| translate language (Translate.Glass dosage) ]
        , text " "
        , text <| translate language Translate.AfterEachLiquidStool
        , text "."
        ]


viewAcuteIllnessOngoingTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> OngoingTreatmentData -> List (Html Msg)
viewAcuteIllnessOngoingTreatment language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ OngoingTreatmentReview ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        OngoingTreatmentReview ->
                            ( "treatment-review"
                            , isJust measurements.treatmentReview
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveOngoingTreatmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.OngoingTreatmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, ongoingTreatmentTasksCompletedFromTotal language currentDate measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                OngoingTreatmentReview ->
                    measurements.treatmentOngoing
                        |> getMeasurementValueFunc
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewOngoingTreatmentReviewForm language currentDate

        actions =
            let
                saveMsg =
                    case data.activeTask of
                        OngoingTreatmentReview ->
                            SaveOngoingTreatmentReview personId measurements.treatmentOngoing
            in
            div [ class "actions treatment-ongoing" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveMsg
                    ]
                    [ text <| translate language Translate.Save ]
                ]
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


viewOngoingTreatmentReviewForm : Language -> NominalDate -> OngoingTreatmentReviewForm -> Html Msg
viewOngoingTreatmentReviewForm language currentDate form =
    let
        ( inputs, _ ) =
            treatmentReviewInputsAndTasks language
                currentDate
                SetOngoingTreatmentReviewBoolInput
                SetReasonForNotTaking
                SetTotalMissedDoses
                SetAdverseEvent
                form
    in
    div [ class "ui form ongoing-treatment-review" ]
        inputs


viewAcuteIllnessDangerSigns : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> DangerSignsData -> List (Html Msg)
viewAcuteIllnessDangerSigns language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ ReviewDangerSigns ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        ReviewDangerSigns ->
                            ( "danger-signs"
                            , isJust measurements.dangerSigns
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveDangerSignsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.DangerSignsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, dangerSignsTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                ReviewDangerSigns ->
                    measurements.dangerSigns
                        |> getMeasurementValueFunc
                        |> reviewDangerSignsFormWithDefault data.reviewDangerSignsForm
                        |> viewReviewDangerSignsForm language currentDate measurements

        actions =
            let
                saveMsg =
                    case data.activeTask of
                        ReviewDangerSigns ->
                            SaveReviewDangerSigns personId measurements.dangerSigns
            in
            div [ class "actions treatment-ongoing" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveMsg
                    ]
                    [ text <| translate language Translate.Save ]
                ]
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


viewReviewDangerSignsForm : Language -> NominalDate -> AcuteIllnessMeasurements -> ReviewDangerSignsForm -> Html Msg
viewReviewDangerSignsForm language currentDate measurements form =
    div [ class "ui form danger-signs" ]
        [ viewQuestionLabel language Translate.ConditionImprovingQuestion
        , viewBoolInput
            language
            form.conditionImproving
            SetConditionImproving
            "conditionImproving"
            Nothing
        , viewQuestionLabel language Translate.HaveAnyOfTheFollowingQuestion
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            [ DangerSignUnableDrinkSuck
            , DangerSignVomiting
            , DangerSignConvulsions
            , DangerSignLethargyUnconsciousness
            , DangerSignRespiratoryDistress
            , DangerSignSpontaneousBleeding
            , DangerSignBloodyDiarrhea
            , DangerSignNewSkinRash
            , NoAcuteIllnessDangerSign
            ]
            []
            (form.symptoms |> Maybe.withDefault [])
            Nothing
            SetDangerSign
            Translate.AcuteIllnessDangerSign
        ]


viewHealthEducationForm : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate maybeDiagnosis form =
    let
        healthEducationSection =
            maybeDiagnosis
                |> Maybe.map
                    (\diagnosis ->
                        let
                            providedHealthEducation =
                                form.educationForDiagnosis
                                    |> Maybe.withDefault True

                            reasonForNotProvidingHealthEducation =
                                if not providedHealthEducation then
                                    let
                                        reasonForNotProvidingHealthEducationOptions =
                                            [ PatientNeedsEmergencyReferral
                                            , ReceivedEmergencyCase
                                            , LackOfAppropriateEducationUserGuide
                                            , PatientRefused
                                            ]
                                    in
                                    [ viewQuestionLabel language Translate.WhyNot
                                    , viewCheckBoxSelectInput language
                                        reasonForNotProvidingHealthEducationOptions
                                        []
                                        form.reasonForNotProvidingHealthEducation
                                        SetReasonForNotProvidingHealthEducation
                                        Translate.ReasonForNotProvidingHealthEducation
                                    ]

                                else
                                    []
                        in
                        div [ class "label" ]
                            [ text <| translate language Translate.ProvidedPreventionEducationQuestion
                            , text " "
                            , text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis
                            , text "?"
                            , viewBoolInput
                                language
                                form.educationForDiagnosis
                                SetProvidedEducationForDiagnosis
                                "education-for-diagnosis"
                                Nothing
                            ]
                            :: reasonForNotProvidingHealthEducation
                    )
                |> Maybe.withDefault [ emptyNode ]
    in
    maybeDiagnosis
        |> Maybe.map
            (\diagnosis ->
                div [ class "ui form health-education" ] <|
                    [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                    , div [ class "instructions" ]
                        [ viewHealthEducationLabel language Translate.ProvideHealthEducation (Translate.AcuteIllnessDiagnosis diagnosis) "icon-open-book" Nothing
                        ]
                    ]
                        ++ healthEducationSection
            )
        |> Maybe.withDefault emptyNode


viewSymptomsReliefForm : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> HealthEducationForm -> Html Msg
viewSymptomsReliefForm language currentDate maybeDiagnosis form =
    let
        viewSymptomRelief symptomsRelief =
            li [] [ text <| translate language <| Translate.SymptomRelief symptomsRelief ]

        symptomsReliefList =
            [ SymptomReliefParacetamol
            , SymptomReliefVitaminC
            , SymptomReliefPaidoterineSyrup
            , SymptomReliefCoughMixture
            ]
    in
    div [ class "ui form symptoms-relief" ] <|
        [ viewCustomLabel language Translate.AcuteIllnessLowRiskCaseHelper "." "instructions"
        , viewLabel language Translate.RecommendedSymptomRelief
        , ul [] <|
            List.map viewSymptomRelief symptomsReliefList
        , viewQuestionLabel language Translate.ProvidedSymtomReliefGuidanceQuestion
        , viewBoolInput
            language
            form.educationForDiagnosis
            SetProvidedEducationForDiagnosis
            "education-for-diagnosis"
            Nothing
        ]


viewHealthEducationLabel : Language -> TranslationId -> TranslationId -> String -> Maybe NominalDate -> Html any
viewHealthEducationLabel language actionTranslationId diagnosisTranslationId iconClass maybeDate =
    let
        message =
            div [] <|
                [ text <| translate language actionTranslationId
                , text " "
                , span [] [ text <| translate language diagnosisTranslationId ]
                ]
                    ++ renderDatePart language maybeDate
                    ++ [ text "." ]
    in
    viewInstructionsLabel iconClass message


viewFollowUpForm : Language -> NominalDate -> Bool -> FollowUpForm -> Html Msg
viewFollowUpForm language currentDate isChw form =
    let
        ( headerHelper, label ) =
            if isChw then
                ( [], Translate.FollowUpByChwLabel )

            else
                ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                  , div [ class "instructions" ]
                        [ viewFollowUpLabel language Translate.AlertChwToFollowUp "icon-house"
                        ]
                  ]
                , Translate.FollowUpLabel
                )
    in
    div [ class "ui form follow-up" ] <|
        headerHelper
            ++ [ viewLabel language label
               , viewCheckBoxSelectInput language
                    [ OneDay, ThreeDays, OneWeek, TwoWeeks, FollowUpNotNeeded ]
                    []
                    form.option
                    SetFollowUpOption
                    Translate.FollowUpOption
               ]


viewFollowUpLabel : Language -> TranslationId -> String -> Html any
viewFollowUpLabel language actionTranslationId iconClass =
    let
        message =
            div [] [ text <| translate language actionTranslationId ++ "." ]
    in
    viewInstructionsLabel iconClass message


viewContactsTracingForm : Language -> NominalDate -> Site -> GeoInfo -> ModelIndexedDb -> Bool -> ContactsTracingForm -> Html Msg
viewContactsTracingForm language currentDate site geoInfo db contactsTracingFinished form =
    let
        content =
            case form.state of
                ContactsTracingFormSummary ->
                    viewContactsTracingFormSummary language currentDate db contactsTracingFinished form.contacts

                ContactsTracingFormSearchParticipants data ->
                    let
                        recordedContacts =
                            Maybe.map Dict.keys form.contacts
                                |> Maybe.withDefault []
                    in
                    viewContactsTracingFormSearchParticipants language currentDate site db recordedContacts data

                ContactsTracingFormRecordContactDetails personId data ->
                    viewContactsTracingFormRecordContactDetails language currentDate personId db data

                ContactsTracingFormRegisterContact data ->
                    viewCreateContactForm language currentDate site geoInfo db data
    in
    div [ class "ui form contacts-tracing" ]
        content


viewContactsTracingFormSummary : Language -> NominalDate -> ModelIndexedDb -> Bool -> Maybe (Dict PersonId ContactTraceItem) -> List (Html Msg)
viewContactsTracingFormSummary language currentDate db contactsTracingFinished contacts =
    let
        contactsForView =
            Maybe.map (Dict.values >> List.map (viewTracedContact language currentDate db contactsTracingFinished))
                contacts
                |> Maybe.withDefault []
    in
    [ viewCustomLabel language Translate.ContactsTracingHelper "." "instructions"
    , div [ class "ui items" ] contactsForView
    , div [ class "dual-action" ]
        [ div
            [ classList
                [ ( "ui primary button", True )
                , ( "disabled", contactsTracingFinished )
                ]
            , onClick <| SetContactsTracingFormState <| ContactsTracingFormSearchParticipants emptySearchParticipantsData
            ]
            [ text <| translate language Translate.AddContact
            ]
        , div
            [ classList
                [ ( "ui primary button", True )
                , ( "disabled", contactsTracingFinished )
                ]
            , onClick SetContactsTracingFinished
            ]
            [ text <| translate language Translate.Finish
            ]
        ]
    ]


viewTracedContact : Language -> NominalDate -> ModelIndexedDb -> Bool -> ContactTraceItem -> Html Msg
viewTracedContact language currentDate db finished contact =
    let
        person =
            Dict.get contact.personId db.people
                |> Maybe.andThen RemoteData.toMaybe

        name =
            generateFullName contact.firstName contact.secondName

        birthDate =
            Maybe.andThen .birthDate person

        village =
            Maybe.andThen .village person

        avatarUrl =
            Maybe.andThen .avatarUrl person

        defaultIcon =
            Maybe.map (defaultIconForPerson currentDate) person
                |> Maybe.withDefault "mother"

        action =
            showIf (not finished) <|
                div [ class "action" ]
                    [ div
                        [ class "action-icon-wrapper"
                        , onClick <| DeleteTracedContact contact.personId
                        ]
                        [ span [ class "icon-checked-in" ] [] ]
                    ]

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2 [ class "ui header" ]
                        [ text name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ Maybe.map (renderDate language) birthDate
                                |> Maybe.withDefault "--/--/--"
                                |> text
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ village |> Maybe.withDefault "" |> text ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.DateOfContact ++ ": " ]
                        , span [] [ formatDDMMYYYY contact.contactDate |> text ]
                        ]
                    ]
                , action
                ]
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage defaultIcon avatarUrl name 120 120 ]
        , content
        ]


viewContactsTracingFormSearchParticipants : Language -> NominalDate -> Site -> ModelIndexedDb -> List PersonId -> SearchParticipantsData -> List (Html Msg)
viewContactsTracingFormSearchParticipants language currentDate site db existingContacts data =
    let
        searchForm =
            Pages.Utils.viewSearchForm language data.input Translate.PlaceholderSearchContactName SetContactsTracingInput

        searchValue =
            Maybe.withDefault "" data.search

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.filter
                            (\personId _ ->
                                -- Do not display person we already have as a contact.
                                not <| List.member personId existingContacts
                            )
                        )
                    |> Just

        summary =
            Maybe.map (viewWebData language viewSummary identity) results
                |> Maybe.withDefault emptyNode

        viewSummary participants =
            Dict.size participants
                |> Translate.ReportResultsOfContactsSearch
                |> translate language
                |> text

        searchResultsParticipants =
            Maybe.withDefault (Success Dict.empty) results
                |> RemoteData.withDefault Dict.empty
                |> Dict.map viewSearchedParticipant
                |> Dict.values

        viewSearchedParticipant personId person =
            viewContactTracingParticipant language
                currentDate
                personId
                person
                False
                (ContactsTracingFormRecordContactDetails personId emptyRecordContactDetailsData)

        addNewContactSection =
            if String.isEmpty searchValue then
                []

            else
                [ div [ class "register-helper" ]
                    [ text <| translate language Translate.RegisterContactHelper ]
                , div
                    [ class "single-action" ]
                    [ div
                        [ class "ui primary button"
                        , onClick <| SetContactsTracingFormState <| ContactsTracingFormRegisterContact (emptyRegisterContactData site)
                        ]
                        [ text <| translate language Translate.RegisterNewContact ]
                    ]
                ]
    in
    [ div
        [ class "search-top" ]
        [ viewCustomLabel language Translate.SearchEhezaForExistingParticipants "." "search-helper instructions"
        , searchForm
        ]
    , div
        [ class "search-middle" ]
        [ div [ class "results-summary" ]
            [ summary ]
        , div
            [ class "ui unstackable items participants-list" ]
            searchResultsParticipants
        ]
    ]
        ++ addNewContactSection
        ++ [ div
                [ class "single-action" ]
                [ div
                    [ class "ui primary button"
                    , onClick <| SetContactsTracingFormState ContactsTracingFormSummary
                    ]
                    [ text <| translate language Translate.Cancel
                    ]
                ]
           ]


viewContactsTracingFormRecordContactDetails : Language -> NominalDate -> PersonId -> ModelIndexedDb -> RecordContactDetailsData -> List (Html Msg)
viewContactsTracingFormRecordContactDetails language currentDate personId db data =
    Dict.get personId db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                let
                    dateForView =
                        Maybe.map formatDDMMYYYY data.contactDate
                            |> Maybe.withDefault ""

                    dateSelectorConfig =
                        { select = SetContactsTracingDate
                        , close = SetContactsTracingDateSelectorState Nothing
                        , dateFrom = Date.add Days (-1 * covidIsolationPeriod) currentDate
                        , dateTo = currentDate
                        , dateDefault = Nothing
                        }

                    phoneNumberInput =
                        viewTextInput language inputNumber SetContactsTracingPhoneNumber Nothing Nothing

                    inputNumber =
                        Maybe.Extra.or data.phoneNumber person.telephoneNumber
                            |> Maybe.withDefault ""

                    saveButtonAttributes =
                        classList
                            [ ( "ui primary button", True )
                            , ( "disabled", saveDisabled )
                            ]
                            :: saveAction

                    saveAction =
                        Maybe.map
                            (\contactDate ->
                                ContactTraceItem personId
                                    person.firstName
                                    person.secondName
                                    person.gender
                                    inputNumber
                                    contactDate
                                    -- Resolution date is set to the date on which
                                    -- Covid  isolation is completed.
                                    (Date.add Days (covidIsolationPeriod + 1) contactDate)
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    |> SaveTracedContact
                                    |> onClick
                                    |> List.singleton
                            )
                            data.contactDate
                            |> Maybe.withDefault []

                    saveDisabled =
                        isNothing data.contactDate
                in
                [ viewCustomLabel language Translate.ContactsTracingCompleteDetails ":" "instructions"
                , div [ class "ui items" ] <|
                    [ viewContactTracingParticipant language currentDate personId person True (ContactsTracingFormSearchParticipants emptySearchParticipantsData) ]
                , div [ class "contact-detail" ]
                    [ viewLabel language Translate.DateOfContact
                    , div
                        [ class "form-input date"
                        , onClick <| SetContactsTracingDateSelectorState (Just dateSelectorConfig)
                        ]
                        [ text dateForView ]
                    , viewModal <| viewCalendarPopup language data.dateSelectorPopupState data.contactDate
                    ]
                , div [ class "contact-detail" ]
                    [ viewLabel language Translate.TelephoneNumber
                    , div [ class "form-input text" ]
                        [ phoneNumberInput ]
                    ]
                , div [ class "single-action" ]
                    [ div saveButtonAttributes
                        [ text <| translate language Translate.Save ]
                    ]
                ]
            )
        |> Maybe.withDefault []


viewContactTracingParticipant : Language -> NominalDate -> PersonId -> Person -> Bool -> ContactsTracingFormState -> Html Msg
viewContactTracingParticipant language currentDate personId person checked newFormState =
    let
        viewAction =
            let
                checkInClass =
                    if checked then
                        "icon-checked-in"

                    else
                        "icon-check-in"
            in
            div [ class "action" ]
                [ div
                    [ class "action-icon-wrapper"
                    , onClick <| SetContactsTracingFormState newFormState
                    ]
                    [ span [ class checkInClass ] []
                    ]
                ]

        content =
            div [ class "content" ]
                [ div
                    [ class "details" ]
                    [ h2
                        [ class "ui header" ]
                        [ text <| person.name ]
                    , p []
                        [ label [] [ text <| translate language Translate.DOB ++ ": " ]
                        , span []
                            [ person.birthDate
                                |> Maybe.map (renderDate language >> text)
                                |> showMaybe
                            ]
                        ]
                    , p []
                        [ label [] [ text <| translate language Translate.Village ++ ": " ]
                        , span [] [ person.village |> Maybe.withDefault "" |> text ]
                        ]
                    ]
                , viewAction
                ]

        defaultIcon =
            defaultIconForPerson currentDate person
    in
    div
        [ class "item participant-view" ]
        [ div
            [ class "ui image" ]
            [ thumbnailImage defaultIcon person.avatarUrl person.name 120 120 ]
        , content
        ]


viewCreateContactForm : Language -> NominalDate -> Site -> GeoInfo -> ModelIndexedDb -> RegisterContactData -> List (Html Msg)
viewCreateContactForm language currentDate site geoInfo db data =
    let
        request =
            db.postPerson

        emptyOption =
            ( "", "" )

        errors =
            Form.getErrors data

        requestStatus =
            case request of
                Success _ ->
                    -- We only want to report failures. In case
                    -- of success, APP navigates to different page.
                    emptyNode

                Failure err ->
                    div
                        [ class "ui warning message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.BackendError ]
                        , viewError language err
                        ]

                Loading ->
                    viewLoading

                NotAsked ->
                    emptyNode

        demographicFields =
            let
                firstNameInput =
                    Pages.Person.View.viewTextInput language Translate.FirstName Backend.Person.Form.firstName False data

                secondNameInput =
                    Pages.Person.View.viewTextInput language Translate.SecondName Backend.Person.Form.secondName True data

                nameInputs =
                    case site of
                        SiteBurundi ->
                            [ secondNameInput, firstNameInput ]

                        _ ->
                            [ firstNameInput, secondNameInput ]
            in
            List.map (Html.map RegisterContactMsgForm) <|
                nameInputs
                    ++ [ genderInput ]

        genderInput =
            let
                genderField =
                    Form.getFieldAsString Backend.Person.Form.gender data

                label =
                    div [ class "six wide column required" ]
                        [ text <| translate language Translate.GenderLabel ++ ":" ]

                maleOption =
                    [ Form.Input.radioInput "male"
                        genderField
                        [ class "one wide column gender-input" ]
                    , div
                        [ class "three wide column" ]
                        [ text <| translate language (Translate.Gender Male) ]
                    ]

                femaleOption =
                    [ Form.Input.radioInput "female"
                        genderField
                        [ class "one wide column gender-input" ]
                    , div
                        [ class "three wide column" ]
                        [ text <| translate language (Translate.Gender Female) ]
                    ]
            in
            div [ class "ui grid" ] <|
                label
                    :: maleOption
                    ++ femaleOption

        geoLocationInputClass isDisabled =
            "select-input"
                ++ (if isDisabled then
                        " disabled"

                    else
                        ""
                   )

        district =
            Form.getFieldAsString Backend.Person.Form.district data

        sector =
            Form.getFieldAsString Backend.Person.Form.sector data

        cell =
            Form.getFieldAsString Backend.Person.Form.cell data

        viewProvince =
            let
                options =
                    emptyOption
                        :: geoLocationDictToOptions geoInfo.provinces

                disabled =
                    isFormFieldSet district
            in
            Pages.Person.View.viewSelectInput language
                (resolveGeoSructureLabelLevel1 site)
                options
                Backend.Person.Form.province
                "ten"
                (geoLocationInputClass disabled)
                True
                data

        viewDistrict =
            let
                province =
                    Form.getFieldAsString Backend.Person.Form.province data

                options =
                    emptyOption
                        :: (case getValueAsInt province of
                                Nothing ->
                                    []

                                Just provinceId ->
                                    geoInfo.districts
                                        |> filterGeoLocationDictByParent provinceId
                                        |> geoLocationDictToOptions
                           )

                disabled =
                    isFormFieldSet sector
            in
            Pages.Person.View.viewSelectInput language
                (resolveGeoSructureLabelLevel2 site)
                options
                Backend.Person.Form.district
                "ten"
                (geoLocationInputClass disabled)
                True
                data

        viewSector =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt district of
                                Nothing ->
                                    []

                                Just districtId ->
                                    geoInfo.sectors
                                        |> filterGeoLocationDictByParent districtId
                                        |> geoLocationDictToOptions
                           )

                disabled =
                    isFormFieldSet cell
            in
            Pages.Person.View.viewSelectInput language
                (resolveGeoSructureLabelLevel3 site)
                options
                Backend.Person.Form.sector
                "ten"
                (geoLocationInputClass disabled)
                True
                data

        viewCell =
            let
                village =
                    Form.getFieldAsString Backend.Person.Form.village data

                options =
                    emptyOption
                        :: (case getValueAsInt sector of
                                Nothing ->
                                    []

                                Just sectorId ->
                                    geoInfo.cells
                                        |> filterGeoLocationDictByParent sectorId
                                        |> geoLocationDictToOptions
                           )

                disabled =
                    isFormFieldSet village
            in
            Pages.Person.View.viewSelectInput language
                (resolveGeoSructureLabelLevel4 site)
                options
                Backend.Person.Form.cell
                "ten"
                (geoLocationInputClass disabled)
                True
                data

        viewVillage =
            let
                options =
                    emptyOption
                        :: (case getValueAsInt cell of
                                Nothing ->
                                    []

                                Just cellId ->
                                    geoInfo.villages
                                        |> filterGeoLocationDictByParent cellId
                                        |> geoLocationDictToOptions
                           )
            in
            Pages.Person.View.viewSelectInput language
                (resolveGeoSructureLabelLevel5 site)
                options
                Backend.Person.Form.village
                "ten"
                "select-input"
                True
                data

        addressFields =
            [ viewProvince
            , viewDistrict
            , viewSector
            , viewCell
            , viewVillage
            ]

        addressSection =
            [ addressFields
                |> fieldset [ class "registration-form address-info" ]
                |> Html.map RegisterContactMsgForm
            ]

        contactInformationSection =
            [ [ Pages.Person.View.viewTextInput language Translate.TelephoneNumber Backend.Person.Form.phoneNumber False data ]
                |> fieldset [ class "registration-form address-info" ]
                |> Html.map RegisterContactMsgForm
            ]

        submitButton =
            button
                [ classList
                    [ ( "ui button primary fluid", True )
                    , ( "loading", RemoteData.isLoading request )
                    , ( "disabled", RemoteData.isLoading request )
                    ]
                , type_ "submit"
                , onClick Form.Submit
                ]
                [ text <| translate language Translate.Save ]

        cancelButton =
            button
                [ class "ui button primary"
                , onClick <| SetContactsTracingFormState <| ContactsTracingFormSearchParticipants emptySearchParticipantsData
                ]
                [ text <| translate language Translate.Cancel ]

        formContent =
            fieldset [ class "registration-form" ]
                demographicFields
                :: contactInformationSection
                ++ addressSection
                ++ [ div [ class "dual-action" ]
                        [ submitButton
                            |> Html.map RegisterContactMsgForm
                        , cancelButton
                        ]

                   -- Note that these are hidden by deafult by semantic-ui ... the
                   -- class of the "form" controls whether they are shown.
                   , requestStatus
                   , div
                        [ class "ui error message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.ValidationErrors ]
                        , List.map (viewFormError language) errors
                            |> ul []
                        ]
                   ]
    in
    [ div
        [ classList
            [ ( "ui form registration", True )
            , ( "error", Form.isSubmitted data && not (List.isEmpty errors) )
            , ( "success", RemoteData.isSuccess request )
            , ( "warning", RemoteData.isFailure request )
            ]
        ]
        formContent
    ]
