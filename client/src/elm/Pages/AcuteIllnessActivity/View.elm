module Pages.AcuteIllnessActivity.View exposing (view)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Encoder exposing (malariaRapidTestResultAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (..)
import Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (..)
import Pages.AcuteIllnessEncounter.View exposing (viewPersonDetailsWithAlert)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , taskListCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCheckBoxValueInput
        , viewCustomLabel
        , viewEverySetInput
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromPhotoUrl
        , viewPreviousMeasurement
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        data =
            generateAssembledData id db

        content =
            viewWebData language (viewContent language currentDate id activity model) identity data
    in
    div [ class "page-activity acute-illness" ] <|
        [ viewHeader language id activity
        , viewWebData language (viewContent language currentDate id activity model) identity data
        , viewModal <|
            warningPopup language
                model.warningPopupState
                SetWarningPopupState
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.AcuteIllnessActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Model -> AssembledData -> Html Msg
viewContent language currentDate id activity model data =
    let
        diagnosis =
            resolveAcuteIllnessDiagnosis data.measurements
    in
    (viewPersonDetailsWithAlert language currentDate data.person diagnosis model.showAlertsDialog SetAlertsDialogState
        :: viewActivity language currentDate id activity diagnosis data model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> Maybe AcuteIllnessDiagnosis -> (Maybe AcuteIllnessDiagnosis -> msg) -> Maybe (Html msg)
warningPopup language maybeDiagnosis setStateMsg =
    maybeDiagnosis
        |> Maybe.map
            (\diagnosis ->
                let
                    content =
                        case diagnosis of
                            DiagnosisCovid19 ->
                                [ div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseIsolate ]
                                , div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseContactHC ]
                                ]

                            DiagnosisMalariaComplicated ->
                                []

                            DiagnosisMalariaUncomplicated ->
                                []
                in
                div [ class "ui active modal warning-popup" ]
                    [ div [ class "content" ] <|
                        [ div [ class "popup-heading-wrapper" ]
                            [ img [ src "assets/images/exclamation-red.png" ] []
                            , div [ class "popup-heading" ] [ text <| translate language Translate.Warning ++ "!" ]
                            ]
                        , div [ class "popup-title" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
                        ]
                            ++ content
                    , div
                        [ class "actions" ]
                        [ button
                            [ class "ui primary fluid button"
                            , onClick <| setStateMsg Nothing
                            ]
                            [ text <| translate language Translate.Continue ]
                        ]
                    ]
            )


viewActivity : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Maybe AcuteIllnessDiagnosis -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate id activity diagnosis data model =
    let
        personId =
            data.participant.person

        measurements =
            data.measurements
    in
    case activity of
        AcuteIllnessSymptoms ->
            viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) model.symptomsData

        AcuteIllnessPhysicalExam ->
            viewAcuteIllnessPhysicalExam language currentDate id ( personId, measurements ) model.physicalExamData

        AcuteIllnessPriorTreatment ->
            viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) model.priorTreatmentData

        AcuteIllnessLaboratory ->
            viewAcuteIllnessLaboratory language currentDate id ( personId, data.person, measurements ) model.laboratoryData

        AcuteIllnessExposure ->
            viewAcuteIllnessExposure language currentDate id ( personId, measurements ) (diagnosis == Just DiagnosisCovid19) model.exposureData


viewAcuteIllnessSymptomsContent : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> SymptomsData -> List (Html Msg)
viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessSymptoms

        tasks =
            [ SymptomsGeneral, SymptomsRespiratory, SymptomsGI ]

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
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveSymptomsTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
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
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                SymptomsGeneral ->
                    measurements.symptomsGeneral
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
                        |> viewSymptomsGeneralForm language currentDate measurements

                SymptomsRespiratory ->
                    measurements.symptomsRespiratory
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
                        |> viewSymptomsRespiratoryForm language currentDate measurements

                SymptomsGI ->
                    measurements.symptomsGI
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGIFormWithDefault data.symptomsGIForm
                        |> viewSymptomsGIForm language currentDate measurements

        getNextTask currentTask =
            case currentTask of
                SymptomsGeneral ->
                    [ SymptomsRespiratory, SymptomsGI ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                SymptomsRespiratory ->
                    [ SymptomsGI, SymptomsGeneral ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                SymptomsGI ->
                    [ SymptomsGeneral, SymptomsRespiratory ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveMsg =
                    case data.activeTask of
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
    viewCheckBoxValueInput language
        allSymptomsRespiratorySigns
        form.signs
        ToggleSymptomsRespiratorySign
        SetSymptomsRespiratorySignValue
        Translate.SymptomsRespiratorySign
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form respiratory" ]


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


viewAcuteIllnessPhysicalExam : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> PhysicalExamData -> List (Html Msg)
viewAcuteIllnessPhysicalExam language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessPhysicalExam

        tasks =
            [ PhysicalExamVitals, PhysicalExamAcuteFindings ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        PhysicalExamVitals ->
                            ( "physical-exam-vitals"
                            , isJust measurements.vitals
                            )

                        PhysicalExamAcuteFindings ->
                            ( "acute-findings"
                            , isJust measurements.acuteFindings
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActivePhysicalExamTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PhysicalExamTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, physicalExamTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                PhysicalExamVitals ->
                    measurements.vitals
                        |> Maybe.map (Tuple.second >> .value)
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language currentDate measurements

                PhysicalExamAcuteFindings ->
                    measurements.acuteFindings
                        |> Maybe.map (Tuple.second >> .value)
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
                        |> viewAcuteFindingsForm language currentDate measurements

        getNextTask currentTask =
            case currentTask of
                PhysicalExamVitals ->
                    [ PhysicalExamAcuteFindings ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                PhysicalExamAcuteFindings ->
                    [ PhysicalExamVitals ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveMsg =
                    case data.activeTask of
                        PhysicalExamVitals ->
                            SaveVitals personId measurements.vitals nextTask

                        PhysicalExamAcuteFindings ->
                            SaveAcuteFindings personId measurements.acuteFindings nextTask
            in
            div [ class "actions symptoms" ]
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


viewVitalsForm : Language -> NominalDate -> AcuteIllnessMeasurements -> VitalsForm -> Html Msg
viewVitalsForm language currentDate measurements form =
    let
        respiratoryRatePreviousValue =
            -- Todo
            -- resolvePreviousValue assembled .vitals .respiratoryRate
            --     |> Maybe.map toFloat
            Nothing

        bodyTemperaturePreviousValue =
            -- Todo
            -- resolvePreviousValue assembled .vitals .bodyTemperature
            Nothing
    in
    div [ class "ui form examination vitals" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.RespiratoryRate ]
            , div [ class "four wide column" ]
                [-- viewConditionalAlert form.respiratoryRate
                 --    [ [ (>) 12 ], [ (<) 30 ] ]
                 --    [ [ (<=) 21, (>=) 30 ] ]
                ]
            ]
        , viewMeasurementInput
            language
            (Maybe.map toFloat form.respiratoryRate)
            SetVitalsResporatoryRate
            "respiratory-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language respiratoryRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BodyTemperature ]
            , div [ class "four wide column" ]
                [-- viewConditionalAlert form.bodyTemperature
                 --     [ [ (>) 35 ], [ (<) 37.5 ] ]
                 --     []
                ]
            ]
        , viewMeasurementInput
            language
            form.bodyTemperature
            SetVitalsBodyTemperature
            "body-temperature"
            Translate.Celsius
        , viewPreviousMeasurement language bodyTemperaturePreviousValue Translate.Celsius
        ]


viewAcuteFindingsForm : Language -> NominalDate -> AcuteIllnessMeasurements -> AcuteFindingsForm -> Html Msg
viewAcuteFindingsForm language currentDate measurements form_ =
    let
        form =
            measurements.acuteFindings
                |> Maybe.map (Tuple.second >> .value)
                |> acuteFindingsFormWithDefault form_
    in
    div [ class "ui form examination acute-findings" ]
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


viewAcuteIllnessLaboratory : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, Person, AcuteIllnessMeasurements ) -> LaboratoryData -> List (Html Msg)
viewAcuteIllnessLaboratory language currentDate id ( personId, person, measurements ) data =
    let
        activity =
            AcuteIllnessLaboratory

        diagnosis =
            resolveAcuteIllnessDiagnosis measurements

        tasks =
            resolveLaboratoryTasks diagnosis

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        LaboratoryMalariaTesting ->
                            ( "laboratory-malaria-testing"
                            , isJust measurements.malariaTesting
                            )

                        LaboratoryMedicationDistribution ->
                            ( "laboratory-medication-distribution"
                            , isJust measurements.medicationDistribution
                            )

                        LaboratorySendToHC ->
                            ( "laboratory-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLaboratoryTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.LaboratoryTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, laboratoryTasksCompletedFromTotal diagnosis measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                LaboratoryMalariaTesting ->
                    measurements.malariaTesting
                        |> Maybe.map (Tuple.second >> .value)
                        |> malariaTestingFormWithDefault data.malariaTestingForm
                        |> viewMalariaTestingForm language currentDate

                LaboratoryMedicationDistribution ->
                    measurements.medicationDistribution
                        |> Maybe.map (Tuple.second >> .value)
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> viewMedicationDistributionForm language currentDate person diagnosis

                LaboratorySendToHC ->
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHCForm language currentDate

        getNextTask currentTask =
            case currentTask of
                LaboratoryMalariaTesting ->
                    []

                -- Todo:
                LaboratoryMedicationDistribution ->
                    []

                -- Todo:
                LaboratorySendToHC ->
                    []

        actions =
            let
                saveMsg =
                    case data.activeTask of
                        LaboratoryMalariaTesting ->
                            SaveMalariaTesting personId measurements.malariaTesting

                        LaboratorySendToHC ->
                            SaveSendToHC personId measurements.sendToHC

                        LaboratoryMedicationDistribution ->
                            SaveMedicationDistribution personId measurements.medicationDistribution
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


viewMalariaTestingForm : Language -> NominalDate -> MalariaTestingForm -> Html Msg
viewMalariaTestingForm language currentDate form =
    let
        emptyOption =
            if isNothing form.rapidTestResult then
                option
                    [ value ""
                    , selected (form.rapidTestResult == Nothing)
                    ]
                    [ text "" ]

            else
                emptyNode

        resultInput =
            emptyOption
                :: ([ RapidTestNegative, RapidTestPositive, RapidTestIndeterminate ]
                        |> List.map
                            (\result ->
                                option
                                    [ value (malariaRapidTestResultAsString result)
                                    , selected (form.rapidTestResult == Just result)
                                    ]
                                    [ text <| translate language <| Translate.MalariaRapidTestResult result ]
                            )
                   )
                |> select [ onInput SetRapidTestResult, class "form-input rapid-test-result" ]
    in
    div [ class "ui form laboratory malaria-testing" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.MalariaRapidDiagnosticTest ]
            , div [ class "four wide column" ]
                [-- viewConditionalAlert form.respiratoryRate
                 --    [ [ (>) 12 ], [ (<) 30 ] ]
                 --    [ [ (<=) 21, (>=) 30 ] ]
                ]
            ]
        , resultInput
        ]


viewSendToHCForm : Language -> NominalDate -> SendToHCForm -> Html Msg
viewSendToHCForm language currentDate form =
    div [ class "ui form send-to-hc" ]
        [ div [ class "ui grid" ]
            [ div [ class "sixteen wide column" ]
                [ viewQuestionLabel language Translate.ReferredPatientToHealthCenterQuestion ]
            ]
        , viewBoolInput
            language
            form.referToHealthCenter
            SetReferToHealthCenter
            "refer-to-hc"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "sixteen wide column" ]
                [ viewQuestionLabel language Translate.HandedReferralFormQuestion ]
            ]
        , viewBoolInput
            language
            form.handReferralForm
            SetHandReferralForm
            "hand-referral-form"
            Nothing
        ]


viewMedicationDistributionForm : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> MedicationDistributionForm -> Html Msg
viewMedicationDistributionForm language currentDate person diagnosis form =
    case diagnosis of
        Just DiagnosisMalariaUncomplicated ->
            let
                instructions =
                    resolveCoartemDosage currentDate person
                        |> Maybe.map
                            (\dosage ->
                                div [ class "instructions coartem" ]
                                    [ div [ class "header" ]
                                        [ text <| translate language Translate.Administer
                                        , text " "
                                        , span [] [ text <| translate language (Translate.MedicationDistributionSign Coartem) ]
                                        , text ":"
                                        ]
                                    , div [ class "dosage" ]
                                        [ span [] [ text <| translate language (Translate.TabletSinglePlural dosage) ]
                                        , text " "
                                        , text <| translate language Translate.ByMouthTwiceADayFor3Days
                                        , text "."
                                        ]
                                    ]
                            )
                        |> Maybe.withDefault emptyNode

                questionLabel =
                    translate language Translate.AdministeredMedicationQuestion
                        ++ " "
                        ++ translate language (Translate.MedicationDistributionSign Coartem)
                        ++ " "
                        ++ translate language Translate.ToThePatient
                        ++ "?"
            in
            div [ class "ui form medication-distribution" ]
                [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                , instructions
                , div [ class "ui grid" ]
                    [ div [ class "sixteen wide column" ]
                        [ text questionLabel ]
                    ]
                , viewEverySetInput
                    language
                    form.signs
                    Coartem
                    ToggleMedicationDistributionSign
                    "coartem-medication"
                    Nothing
                ]

        _ ->
            emptyNode


viewAcuteIllnessExposure : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> Bool -> ExposureData -> List (Html Msg)
viewAcuteIllnessExposure language currentDate id ( personId, measurements ) suspectedCovid19 data =
    let
        activity =
            AcuteIllnessExposure

        tasks =
            resolveExposureTasks measurements suspectedCovid19

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

                        ExposureIsolation ->
                            ( "exposure-isolation"
                            , isJust measurements.isolation
                            )

                        ExposureContactHC ->
                            ( "exposure-contact-hc"
                            , isJust measurements.hcContact
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
                [ a attributes
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
                        |> Maybe.map (Tuple.second >> .value)
                        |> travelHistoryFormWithDefault data.travelHistoryForm
                        |> viewTravelHistoryForm language currentDate measurements

                ExposureExposure ->
                    measurements.exposure
                        |> Maybe.map (Tuple.second >> .value)
                        |> exposureFormWithDefault data.exposureForm
                        |> viewExposureForm language currentDate measurements

                ExposureIsolation ->
                    measurements.isolation
                        |> Maybe.map (Tuple.second >> .value)
                        |> isolationFormWithDefault data.isolationForm
                        |> viewIsolationForm language currentDate measurements

                ExposureContactHC ->
                    measurements.hcContact
                        |> Maybe.map (Tuple.second >> .value)
                        |> hcContactFormWithDefault data.hcContactForm
                        |> viewHCContactForm language currentDate measurements

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

                ExposureIsolation ->
                    [ ExposureContactHC ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                ExposureContactHC ->
                    [ ExposureIsolation ]
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

                        ExposureIsolation ->
                            SaveIsolation personId measurements.isolation nextTask

                        ExposureContactHC ->
                            SaveHCContact personId measurements.hcContact nextTask
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
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewQuestionLabel language Translate.TraveledToCOVID19CountryQuestion ]
            , div [ class "four wide column" ]
                [-- viewConditionalAlert form.respiratoryRate
                 --    [ [ (>) 12 ], [ (<) 30 ] ]
                 --    [ [ (<=) 21, (>=) 30 ] ]
                ]
            ]
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
        [ div [ class "ui grid" ]
            [ div [ class "sixteen wide column" ]
                [ viewQuestionLabel language Translate.ContactWithCOVID19SymptomsQuestion ]
            ]
        , div [ class "question-helper" ] [ text <| translate language Translate.ContactWithCOVID19SymptomsHelper ++ "." ]
        , viewBoolInput
            language
            form.covid19Symptoms
            SetCovid19Symptoms
            "covid19-symptoms"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewQuestionLabel language Translate.ContactWithSimilarSymptomsQuestion ]
            , div [ class "four wide column" ]
                [-- viewConditionalAlert form.respiratoryRate
                 --    [ [ (>) 12 ], [ (<) 30 ] ]
                 --    [ [ (<=) 21, (>=) 30 ] ]
                ]
            ]
        , viewBoolInput
            language
            form.similarSymptoms
            SetSimilarSymptoms
            "similar-symptoms"
            Nothing
        ]


viewIsolationForm : Language -> NominalDate -> AcuteIllnessMeasurements -> IsolationForm -> Html Msg
viewIsolationForm language currentDate measurements form =
    let
        patientIsolatedInput =
            [ div [ class "ui grid" ]
                [ div [ class "twelve wide column" ]
                    [ viewQuestionLabel language Translate.PatientIsolatedQuestion ]
                , div [ class "four wide column" ]
                    [-- viewConditionalAlert form.respiratoryRate
                     --    [ [ (>) 12 ], [ (<) 30 ] ]
                     --    [ [ (<=) 21, (>=) 30 ] ]
                    ]
                ]
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
                    [ div [ class "ui grid" ]
                        [ div [ class "twelve wide column" ]
                            [ viewQuestionLabel language Translate.SignOnDoorPostedQuestion ]
                        , div [ class "four wide column" ]
                            [-- viewConditionalAlert form.respiratoryRate
                             --    [ [ (>) 12 ], [ (<) 30 ] ]
                             --    [ [ (<=) 21, (>=) 30 ] ]
                            ]
                        ]
                    , viewBoolInput
                        language
                        form.signOnDoor
                        SetSignOnDoor
                        "sign-on-door"
                        Nothing
                    ]
                        ++ healthEducationInput

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
            [ div [ class "ui grid" ]
                [ div [ class "twelve wide column" ]
                    [ viewQuestionLabel language Translate.HealthEducationProvidedQuestion ]
                , div [ class "four wide column" ]
                    [-- viewConditionalAlert form.respiratoryRate
                     --    [ [ (>) 12 ], [ (<) 30 ] ]
                     --    [ [ (<=) 21, (>=) 30 ] ]
                    ]
                ]
            , viewBoolInput
                language
                form.healthEducation
                SetHealthEducation
                "health-education"
                Nothing
            ]
    in
    patientIsolatedInput
        ++ derivedInputs
        |> div [ class "ui form exposure isolation" ]


viewHCContactForm : Language -> NominalDate -> AcuteIllnessMeasurements -> HCContactForm -> Html Msg
viewHCContactForm language currentDate measurements form =
    let
        contactedHCInput =
            [ div [ class "ui grid" ]
                [ div [ class "twelve wide column" ]
                    [ viewQuestionLabel language Translate.ContactedHCQuestion ]
                , div [ class "four wide column" ]
                    [-- viewConditionalAlert form.respiratoryRate
                     --    [ [ (>) 12 ], [ (<) 30 ] ]
                     --    [ [ (<=) 21, (>=) 30 ] ]
                    ]
                ]
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
                        hcRespnonseInput =
                            [ viewQuestionLabel language Translate.HCResponseQuestion
                            , viewCheckBoxSelectCustomInput language
                                [ SendAmbulance, HomeIsolation, ComeToHealthCenter, ChwMonitoring ]
                                []
                                form.recomendations
                                SetHCRecommendation
                                (viewHCRecomendation language)
                            ]

                        hcRespnonsePerionInput =
                            [ viewQuestionLabel language Translate.HCResponsePeriodQuestion
                            , viewCheckBoxSelectInput language
                                [ LessThan30Min, Between30min1Hour, Between1Hour2Hour, Between2Hour1Day ]
                                []
                                form.responsePeriod
                                SetResponsePeriod
                                Translate.ResponsePeriod
                            ]

                        derivedInput =
                            form.recomendations
                                |> Maybe.map
                                    (\recomendations ->
                                        if recomendations == SendAmbulance then
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
                    hcRespnonseInput ++ hcRespnonsePerionInput ++ derivedInput

                _ ->
                    []
    in
    contactedHCInput
        ++ derivedInputs
        |> div [ class "ui form exposure hc-contact" ]


viewHCRecomendation : Language -> HCRecomendation -> Html Msg
viewHCRecomendation language recomendation =
    let
        riskLevel =
            case recomendation of
                SendAmbulance ->
                    Translate.HighRiskCase

                HomeIsolation ->
                    Translate.HighRiskCase

                ComeToHealthCenter ->
                    Translate.LowRiskCase

                ChwMonitoring ->
                    Translate.LowRiskCase

                HCRecomendationNotApplicable ->
                    Translate.LowRiskCase
    in
    label []
        [ translate language Translate.HealthCenterDetermined |> text
        , span [ class "strong" ] [ translate language riskLevel |> text ]
        , translate language Translate.And |> text
        , span [ class "strong" ] [ Translate.HCRecomendation recomendation |> translate language |> text ]
        ]


viewAcuteIllnessPriorTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> PriorTreatmentData -> List (Html Msg)
viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessPriorTreatment

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
                [ a attributes
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
                        |> Maybe.map (Tuple.second >> .value)
                        |> treatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewTreatmentReviewForm language currentDate measurements

        getNextTask currentTask =
            case currentTask of
                TreatmentReview ->
                    []

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
            { form_ | feverPast6Hours = Just value }

        feverPast6HoursHelpedUpdateFunc value form_ =
            { form_ | feverPast6HoursHelped = Just value }

        malariaTodayUpdateFunc value form_ =
            { form_ | malariaToday = Just value }

        malariaTodayHelpedUpdateFunc value form_ =
            { form_ | malariaTodayHelped = Just value }

        malariaWithinPastMonthUpdateFunc value form_ =
            { form_ | malariaWithinPastMonth = Just value }

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
            [ div [ class "ui grid" ]
                [ div [ class "sixteen wide column" ]
                    [ viewQuestionLabel language Translate.MedicationForFeverPast6HoursQuestion ]
                ]
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
            [ div [ class "ui grid" ]
                [ div [ class "sixteen wide column" ]
                    [ viewQuestionLabel language Translate.MedicationForMalariaWithinPastMonthQuestion ]
                ]
            , viewBoolInput
                language
                form.malariaToday
                (SetTreatmentReviewBoolInput malariaTodayUpdateFunc)
                "malaria-today"
                Nothing
            ]
                ++ malariaTodayHelpedInput

        malariaWithinPastMonth =
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
            [ div [ class "ui grid" ]
                [ div [ class "sixteen wide column" ]
                    [ viewQuestionLabel language Translate.MedicationForMalariaTodayQuestion ]
                ]
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
        ++ malariaWithinPastMonth
        |> div [ class "ui form treatment-review" ]
