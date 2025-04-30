module Pages.AcuteIllness.Activity.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (covidIsolationPeriod, getMeasurementValueFunc, muacValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (defaultIconForPerson, generateFullName, isPersonAFertileWoman)
import Components.PatientsSearchForm.Model
import Components.PatientsSearchForm.Utils exposing (..)
import Components.PatientsSearchForm.View
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
        , renderDatePart
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        , viewAdministeredMedicationCustomLabel
        , vitalsFormWithDefault
        )
import Measurement.View exposing (viewSendToHealthCenterForm, viewSendToHospitalForm)
import Pages.AcuteIllness.Activity.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.AcuteIllness.Activity.Utils exposing (..)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (..)
import Pages.AcuteIllness.Encounter.View exposing (viewPersonDetailsWithAlert, warningPopup)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.View
import Pages.Utils
    exposing
        ( getCurrentReasonForMedicationNonAdministration
        , nonAdministrationReasonToSign
        , resolveActiveTask
        , resolveNextTask
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCheckBoxValueInput
        , viewCustomAction
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewInstructionsLabel
        , viewLabel
        , viewQuestionLabel
        , viewRedAlertForSelect
        , viewTasksCount
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
                , viewCustomAction language closeMsg False Translate.Close
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
            List.map
                (\task ->
                    ( task, symptomsTasksCompletedFromTotal measurements data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just SymptomsGeneral ->
                    getMeasurementValueFunc measurements.symptomsGeneral
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
                        |> viewSymptomsGeneralForm language currentDate measurements

                Just SymptomsRespiratory ->
                    getMeasurementValueFunc measurements.symptomsRespiratory
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
                        |> viewSymptomsRespiratoryForm language currentDate measurements

                Just SymptomsGI ->
                    getMeasurementValueFunc measurements.symptomsGI
                        |> symptomsGIFormWithDefault data.symptomsGIForm
                        |> viewSymptomsGIForm language currentDate measurements

                Nothing ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

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
    , viewTasksCount language tasksCompleted totalTasks
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
            List.map
                (\task ->
                    ( task, physicalExamTasksCompletedFromTotal currentDate isChw person assembled data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just PhysicalExamVitals ->
                    getMeasurementValueFunc measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language
                            currentDate
                            isChw
                            assembled
                        |> List.singleton

                Just PhysicalExamCoreExam ->
                    getMeasurementValueFunc measurements.coreExam
                        |> coreExamFormWithDefault data.coreExamForm
                        |> viewCoreExamForm language currentDate
                        |> List.singleton

                Just PhysicalExamMuac ->
                    let
                        previousValue =
                            resolvePreviousValue assembled .muac muacValueFunc
                    in
                    getMeasurementValueFunc measurements.muac
                        |> muacFormWithDefault data.muacForm
                        |> Measurement.View.viewMuacForm language currentDate site assembled.person previousValue SetMuac

                Just PhysicalExamAcuteFindings ->
                    getMeasurementValueFunc measurements.acuteFindings
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
                        |> viewAcuteFindingsForm language currentDate
                        |> List.singleton

                Just PhysicalExamNutrition ->
                    getMeasurementValueFunc measurements.nutrition
                        |> Pages.AcuteIllness.Activity.Utils.nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate
                        |> List.singleton

                Nothing ->
                    []

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            personId =
                                assembled.participant.person

                            nextTask =
                                resolveNextTask task tasksCompletedFromTotalDict tasks

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
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewVitalsForm : Language -> NominalDate -> Bool -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate isChw assembled form =
    let
        formConfig =
            generateVitalsFormConfig isChw assembled
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewCoreExamForm : Language -> NominalDate -> AcuteIllnessCoreExamForm -> Html Msg
viewCoreExamForm language currentDate form =
    let
        ( inputs, _ ) =
            coreExamFormInutsAndTasks language currentDate form
    in
    div [ class "ui form physical-exam core-exam" ]
        inputs


viewAcuteFindingsForm : Language -> NominalDate -> AcuteFindingsForm -> Html Msg
viewAcuteFindingsForm language currentDate form =
    let
        ( inputs, _ ) =
            acuteFindingsFormInutsAndTasks language currentDate form
    in
    div [ class "ui form physical-exam acute-findings" ]
        inputs


viewNutritionForm : Language -> NominalDate -> AcuteIllnessNutritionForm -> Html Msg
viewNutritionForm language currentDate form =
    let
        ( inputs, _ ) =
            nutritionFormInutsAndTasks language currentDate form
    in
    div [ class "ui form physical-exam nutrition" ]
        inputs


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
            List.map
                (\task ->
                    ( task, laboratoryTasksCompletedFromTotal currentDate assembled.person assembled.measurements data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        covidTestingForm =
            getMeasurementValueFunc assembled.measurements.covidTesting
                |> covidTestingFormWithDefault data.covidTestingForm

        viewForm =
            case activeTask of
                Just LaboratoryMalariaTesting ->
                    getMeasurementValueFunc assembled.measurements.malariaTesting
                        |> malariaTestingFormWithDefault data.malariaTestingForm
                        |> viewMalariaTestingForm language currentDate assembled.person

                Just LaboratoryCovidTesting ->
                    viewCovidTestingForm language currentDate assembled.person covidTestingForm

                Nothing ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

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
    , viewTasksCount language tasksCompleted totalTasks
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
        ( inputs, _ ) =
            malariaTestingFormInputsAndTasks language currentDate person form
    in
    div [ class "ui form laboratory malaria-testing" ]
        inputs


viewCovidTestingForm : Language -> NominalDate -> Person -> CovidTestingForm -> Html Msg
viewCovidTestingForm language currentDate person form =
    let
        ( inputs, _ ) =
            covidTestingFormInputsAndTasks language currentDate person form
    in
    div [ class "ui form laboratory covid-testing" ]
        inputs


viewAcuteIllnessPriorTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> PriorTreatmentData -> List (Html Msg)
viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ TreatmentReview ]

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TreatmentReview ->
                            ( "treatment-review"
                            , isJust measurements.treatmentReview
                            )

                isActive =
                    activeTask == Just task

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
            List.map
                (\task ->
                    ( task, treatmentTasksCompletedFromTotal currentDate measurements data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TreatmentReview ->
                    measurements.treatmentReview
                        |> getMeasurementValueFunc
                        |> treatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewTreatmentReviewForm language currentDate

                _ ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
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


viewTreatmentReviewForm : Language -> NominalDate -> TreatmentReviewForm -> Html Msg
viewTreatmentReviewForm language currentDate form =
    let
        ( inputs, _ ) =
            treatmentReviewFormInutsAndTasks language currentDate form
    in
    div [ class "ui form treatment-review" ]
        inputs


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
            List.map
                (\task ->
                    ( task
                    , nextStepsTasksCompletedFromTotal currentDate
                        isChw
                        assembled.initialEncounter
                        person
                        diagnosis
                        measurements
                        data
                        task
                    )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        call114Form =
            getMeasurementValueFunc measurements.call114
                |> call114FormWithDefault data.call114Form

        viewForm =
            case activeTask of
                Just NextStepsMedicationDistribution ->
                    getMeasurementValueFunc measurements.medicationDistribution
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
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityFunc language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing

                Just NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate diagnosis

                Just NextStepsSymptomsReliefGuidance ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewSymptomsReliefForm language currentDate

                Just NextStepsFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate isChw

                Just NextStepsContactTracing ->
                    getMeasurementValueFunc measurements.contactsTracing
                        |> contactsTracingFormWithDefault data.contactsTracingForm
                        |> viewContactsTracingForm language currentDate site geoInfo db contactsTracingFinished

                Nothing ->
                    emptyNode

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

                            nextTask =
                                resolveNextTask task tasksCompletedFromTotalDict tasksAfterSave

                            tasksAfterSave =
                                case task of
                                    -- If medication is prescribed, but it's out of stock, or partient
                                    -- if alergic, SendToHC should appear, so that patient is dircted to the HC.
                                    -- An exclusion here is when patient is diagnosed with Covid and Pneumonia,
                                    -- where the patient is monitored at home.
                                    NextStepsMedicationDistribution ->
                                        if List.member diagnosis [ Just DiagnosisPneuminialCovid19, Just DiagnosisLowRiskCovid19 ] then
                                            tasks

                                        else
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
                                            if medicationOutOfStockOrPatientAlergic then
                                                [ NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsFollowUp ]

                                            else
                                                [ NextStepsMedicationDistribution, NextStepsFollowUp ]

                                    _ ->
                                        tasks

                            saveMsg =
                                case task of
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


viewMedicationDistributionForm : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> MedicationDistributionForm -> Html Msg
viewMedicationDistributionForm language currentDate person diagnosis form =
    let
        ( inputs, _ ) =
            medicationDistributionFormInutsAndTasks language currentDate person diagnosis form
    in
    div [ class "ui form medication-distribution" ]
        inputs


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


viewAcuteIllnessOngoingTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> OngoingTreatmentData -> List (Html Msg)
viewAcuteIllnessOngoingTreatment language currentDate id ( personId, measurements ) data =
    let
        tasks =
            [ OngoingTreatmentReview ]

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        OngoingTreatmentReview ->
                            ( "treatment-review"
                            , isJust measurements.treatmentReview
                            )

                isActive =
                    activeTask == Just task

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
            List.map
                (\task ->
                    ( task, ongoingTreatmentTasksCompletedFromTotal currentDate measurements data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just OngoingTreatmentReview ->
                    getMeasurementValueFunc measurements.treatmentOngoing
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewOngoingTreatmentReviewForm language currentDate

                _ ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
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

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        ReviewDangerSigns ->
                            ( "danger-signs"
                            , isJust measurements.dangerSigns
                            )

                isActive =
                    activeTask == Just task

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
            List.map
                (\task ->
                    ( task, dangerSignsTasksCompletedFromTotal currentDate measurements data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just ReviewDangerSigns ->
                    getMeasurementValueFunc measurements.dangerSigns
                        |> reviewDangerSignsFormWithDefault data.reviewDangerSignsForm
                        |> viewReviewDangerSignsForm language currentDate

                _ ->
                    emptyNode

        actions =
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
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


viewReviewDangerSignsForm : Language -> NominalDate -> ReviewDangerSignsForm -> Html Msg
viewReviewDangerSignsForm language currentDate form =
    let
        ( inputs, _ ) =
            reviewDangerSignsFormInutsAndTasks language currentDate form
    in
    div [ class "ui form danger-signs" ]
        inputs


viewHealthEducationForm : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate maybeDiagnosis form =
    let
        ( inputs, _ ) =
            healthEducationFormInutsAndTasks language currentDate maybeDiagnosis form
    in
    div [ class "ui form health-education" ]
        inputs


viewSymptomsReliefForm : Language -> NominalDate -> HealthEducationForm -> Html Msg
viewSymptomsReliefForm language currentDate form =
    let
        ( inputs, _ ) =
            symptomsReliefFormInutsAndTasks language currentDate form
    in
    div [ class "ui form symptoms-relief" ]
        inputs


viewFollowUpForm : Language -> NominalDate -> Bool -> FollowUpForm -> Html Msg
viewFollowUpForm language currentDate isChw form =
    let
        ( inputs, _ ) =
            followUpFormInutsAndTasks language currentDate isChw form
    in
    div [ class "ui form follow-up" ]
        inputs


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
            , onClick <|
                SetContactsTracingFormState <|
                    ContactsTracingFormSearchParticipants Components.PatientsSearchForm.Model.emptyModel
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


viewContactsTracingFormSearchParticipants :
    Language
    -> NominalDate
    -> Site
    -> ModelIndexedDb
    -> List PersonId
    -> Components.PatientsSearchForm.Model.Model
    -> List (Html Msg)
viewContactsTracingFormSearchParticipants language currentDate site db existingContacts data =
    let
        searchForm =
            Components.PatientsSearchForm.View.view language data
                |> Html.map Pages.AcuteIllness.Activity.Model.MsgPatientsSearchForm

        searchValue =
            Components.PatientsSearchForm.Utils.getSearchValue data

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Components.PatientsSearchForm.Utils.getSearchResults db data
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
                    [ viewContactTracingParticipant language
                        currentDate
                        personId
                        person
                        True
                        (ContactsTracingFormSearchParticipants Components.PatientsSearchForm.Model.emptyModel)
                    ]
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
                        , span [] [ text <| Maybe.withDefault "" person.village ]
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
            in
            List.map (Html.map RegisterContactMsgForm) <|
                nameInputs
                    ++ [ genderInput ]

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
                , onClick <|
                    SetContactsTracingFormState <|
                        ContactsTracingFormSearchParticipants
                            Components.PatientsSearchForm.Model.emptyModel
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
