module Pages.AcuteIllnessActivity.View exposing (view, viewAdministeredMedicationLabel, viewHCRecommendation, viewOralSolutionPrescription, viewSendToHCActionLabel, viewTabletsPrescription)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Encoder exposing (malariaRapidTestResultAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAFertileWoman)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (getInputConstraintsMuac)
import Measurement.View exposing (viewMuacIndication)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (..)
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (..)
import Pages.AcuteIllnessEncounter.View exposing (viewPersonDetailsWithAlert, warningPopup)
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
        , viewLabel
        , viewMeasurementInput
        , viewNumberInput
        , viewPhotoThumbFromPhotoUrl
        , viewPreviousMeasurement
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (renderDate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity model) identity data


viewHeaderAndContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity model data =
    div [ class "page-activity acute-illness" ]
        [ viewHeader language id activity data.encounter.diagnosis
        , viewContent language currentDate id activity model data
        , viewModal <|
            warningPopup language
                model.warningPopupState
                SetWarningPopupState
        , viewModal <|
            pertinentSymptomsPopup language
                model.showPertinentSymptomsPopup
                (SetPertinentSymptomsPopupState False)
                data.measurements
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> AcuteIllnessDiagnosis -> Html Msg
viewHeader language id activity diagnosis =
    let
        title =
            case activity of
                AcuteIllnessNextSteps ->
                    let
                        prefix =
                            Translate.AcuteIllnessDiagnosis diagnosis
                                |> translate language
                                |> (\diagnosisTitle -> diagnosisTitle ++ ": ")
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
            acuteIllnessDiagnosisToMaybe data.encounter.diagnosis
    in
    (viewPersonDetailsWithAlert language currentDate data.person diagnosis model.showAlertsDialog SetAlertsDialogState
        :: viewActivity language currentDate id activity diagnosis data model
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
                    |> Maybe.map (Tuple.second >> .value)

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
                                        (String.fromInt respiratoryRate ++ " " ++ translate language Translate.BpmUnitLabel)
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
                            >> (Translate.MalariaRapidTestResult
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
                    |> Maybe.map (Tuple.second >> .value)

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
                    |> Maybe.map (Tuple.second >> .value)

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
                List.filterMap identity
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


viewActivity : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Maybe AcuteIllnessDiagnosis -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate id activity diagnosis data model =
    let
        personId =
            data.participant.person

        measurements =
            data.measurements

        isFirstEncounter =
            List.isEmpty data.previousMeasurementsWithDates
    in
    case activity of
        AcuteIllnessSymptoms ->
            viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) model.symptomsData

        AcuteIllnessPhysicalExam ->
            viewAcuteIllnessPhysicalExam language currentDate id data isFirstEncounter model.physicalExamData

        AcuteIllnessPriorTreatment ->
            viewAcuteIllnessPriorTreatment language currentDate id ( personId, measurements ) model.priorTreatmentData

        AcuteIllnessLaboratory ->
            viewAcuteIllnessLaboratory language currentDate id ( personId, data.person, measurements ) model.laboratoryData

        AcuteIllnessExposure ->
            viewAcuteIllnessExposure language currentDate id ( personId, measurements ) model.exposureData

        AcuteIllnessNextSteps ->
            viewAcuteIllnessNextSteps language currentDate id ( personId, data.person, measurements ) isFirstEncounter diagnosis model.nextStepsData

        AcuteIllnessOngoingTreatment ->
            viewAcuteIllnessOngoingTreatment language currentDate id ( personId, measurements ) model.ongoingTreatmentData

        AcuteIllnessDangerSigns ->
            viewAcuteIllnessDangerSigns language currentDate id ( personId, measurements ) model.dangerSignsData


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


viewAcuteIllnessPhysicalExam :
    Language
    -> NominalDate
    -> AcuteIllnessEncounterId
    -> AssembledData
    -> Bool
    -> PhysicalExamData
    -> List (Html Msg)
viewAcuteIllnessPhysicalExam language currentDate id assembled isFirstEncounter data =
    let
        activity =
            AcuteIllnessPhysicalExam

        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            [ PhysicalExamVitals, PhysicalExamMuac, PhysicalExamNutrition, PhysicalExamAcuteFindings ]
                |> List.filter (expectPhysicalExamTask currentDate person isFirstEncounter)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        PhysicalExamVitals ->
                            ( "physical-exam-vitals"
                            , isJust measurements.vitals
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
                        |> viewVitalsForm language currentDate assembled

                PhysicalExamMuac ->
                    measurements.muac
                        |> Maybe.map (Tuple.second >> .value)
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language currentDate assembled

                PhysicalExamAcuteFindings ->
                    measurements.acuteFindings
                        |> Maybe.map (Tuple.second >> .value)
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
                        |> viewAcuteFindingsForm language currentDate measurements

                PhysicalExamNutrition ->
                    measurements.nutrition
                        |> Maybe.map (Tuple.second >> .value)
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate measurements

        getNextTask currentTask =
            case currentTask of
                PhysicalExamVitals ->
                    [ PhysicalExamMuac, PhysicalExamNutrition, PhysicalExamAcuteFindings ]
                        |> List.filter (expectPhysicalExamTask currentDate person isFirstEncounter)
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                PhysicalExamMuac ->
                    [ PhysicalExamNutrition, PhysicalExamAcuteFindings, PhysicalExamVitals ]
                        |> List.filter (expectPhysicalExamTask currentDate person isFirstEncounter)
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                PhysicalExamNutrition ->
                    [ PhysicalExamAcuteFindings, PhysicalExamVitals, PhysicalExamAcuteFindings ]
                        |> List.filter (expectPhysicalExamTask currentDate person isFirstEncounter)
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                PhysicalExamAcuteFindings ->
                    [ PhysicalExamVitals, PhysicalExamMuac, PhysicalExamNutrition ]
                        |> List.filter (expectPhysicalExamTask currentDate person isFirstEncounter)
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
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
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


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form_ =
    let
        measurements =
            assembled.measurements

        form =
            measurements.vitals
                |> Maybe.map (Tuple.second >> .value)
                |> vitalsFormWithDefault form_

        respiratoryRatePreviousValue =
            resolvePreviousValue assembled .vitals .respiratoryRate
                |> Maybe.map toFloat

        bodyTemperaturePreviousValue =
            resolvePreviousValue assembled .vitals .bodyTemperature
    in
    div [ class "ui form physical-exam vitals" ]
        [ viewLabel language Translate.RespiratoryRate
        , viewMeasurementInput
            language
            (Maybe.map toFloat form.respiratoryRate)
            SetVitalsResporatoryRate
            "respiratory-rate"
            Translate.BpmUnitLabel
        , viewPreviousMeasurement language respiratoryRatePreviousValue Translate.BpmUnitLabel
        , div [ class "separator" ] []
        , viewLabel language Translate.BodyTemperature
        , viewMeasurementInput
            language
            form.bodyTemperature
            SetVitalsBodyTemperature
            "body-temperature"
            Translate.Celsius
        , viewPreviousMeasurement language bodyTemperaturePreviousValue Translate.Celsius
        ]


viewMuacForm : Language -> NominalDate -> AssembledData -> MuacForm -> Html Msg
viewMuacForm language currentDate assembled form_ =
    let
        measurements =
            assembled.measurements

        form =
            measurements.muac
                |> Maybe.map (Tuple.second >> .value)
                |> muacFormWithDefault form_

        constraints =
            getInputConstraintsMuac

        previousValue =
            resolvePreviousValue assembled .muac (\(MuacInCm cm) -> cm)
    in
    div [ class "ui form physical-exam muac" ]
        [ viewLabel language Translate.MUAC
        , p [ class "activity-helper" ] [ text <| translate language Translate.MuacHelper ]
        , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.muac
                    SetMuac
                    "muac"
                    Translate.CentimeterShorthand
                ]
            , div
                [ class "five wide column" ]
                [ showMaybe <|
                    Maybe.map (MuacInCm >> muacIndication >> viewMuacIndication language) form.muac
                ]
            ]
        , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
        ]


viewAcuteFindingsForm : Language -> NominalDate -> AcuteIllnessMeasurements -> AcuteFindingsForm -> Html Msg
viewAcuteFindingsForm language currentDate measurements form_ =
    let
        form =
            measurements.acuteFindings
                |> Maybe.map (Tuple.second >> .value)
                |> acuteFindingsFormWithDefault form_
    in
    div [ class "ui form physical-exam acute-findings" ]
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


viewNutritionForm : Language -> NominalDate -> AcuteIllnessMeasurements -> NutritionForm -> Html Msg
viewNutritionForm language currentDate measurements form_ =
    let
        form =
            measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFormWithDefault form_
    in
    div [ class "ui form physical-exam nutrition" ]
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


viewAcuteIllnessLaboratory : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, Person, AcuteIllnessMeasurements ) -> LaboratoryData -> List (Html Msg)
viewAcuteIllnessLaboratory language currentDate id ( personId, person, measurements ) data =
    let
        activity =
            AcuteIllnessLaboratory

        tasks =
            [ LaboratoryMalariaTesting ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        LaboratoryMalariaTesting ->
                            ( "laboratory-malaria-testing"
                            , isJust measurements.malariaTesting
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
                        ( task, laboratoryTasksCompletedFromTotal measurements data task )
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
                        |> viewMalariaTestingForm language currentDate person

        actions =
            let
                saveMsg =
                    case data.activeTask of
                        LaboratoryMalariaTesting ->
                            SaveMalariaTesting personId measurements.malariaTesting
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


viewMalariaTestingForm : Language -> NominalDate -> Person -> MalariaTestingForm -> Html Msg
viewMalariaTestingForm language currentDate person form =
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
                :: ([ RapidTestNegative, RapidTestPositive, RapidTestIndeterminate, RapidTestUnableToRun ]
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

        testResultPositive =
            form.rapidTestResult == Just RapidTestPositive || form.rapidTestResult == Just RapidTestPositiveAndPregnant

        isPregnantInput =
            if testResultPositive && isPersonAFertileWoman currentDate person then
                [ viewQuestionLabel language Translate.CurrentlyPregnant
                , viewBoolInput
                    language
                    form.isPregnant
                    SetIsPregnant
                    "is-pregnant"
                    Nothing
                ]

            else
                []
    in
    div [ class "ui form laboratory malaria-testing" ] <|
        [ viewLabel language Translate.MalariaRapidDiagnosticTest
        , resultInput
        ]
            ++ isPregnantInput


viewAcuteIllnessExposure : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> ExposureData -> List (Html Msg)
viewAcuteIllnessExposure language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessExposure

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


viewAcuteIllnessNextSteps : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, Person, AcuteIllnessMeasurements ) -> Bool -> Maybe AcuteIllnessDiagnosis -> NextStepsData -> List (Html Msg)
viewAcuteIllnessNextSteps language currentDate id ( personId, person, measurements ) isFirstEncounter diagnosis data =
    let
        activity =
            AcuteIllnessNextSteps

        tasks =
            resolveNextStepsTasks currentDate person isFirstEncounter diagnosis measurements

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

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
                    , text <| translate language (Translate.NextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal diagnosis measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsIsolation ->
                    measurements.isolation
                        |> Maybe.map (Tuple.second >> .value)
                        |> isolationFormWithDefault data.isolationForm
                        |> viewIsolationForm language currentDate measurements

                Just NextStepsContactHC ->
                    measurements.hcContact
                        |> Maybe.map (Tuple.second >> .value)
                        |> hcContactFormWithDefault data.hcContactForm
                        |> viewHCContactForm language currentDate measurements

                Just NextStepsCall114 ->
                    measurements.call114
                        |> Maybe.map (Tuple.second >> .value)
                        |> call114FormWithDefault data.call114Form
                        |> viewCall114Form language currentDate measurements

                Just NextStepsMedicationDistribution ->
                    measurements.medicationDistribution
                        |> Maybe.map (Tuple.second >> .value)
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> viewMedicationDistributionForm language currentDate person diagnosis

                Just NextStepsSendToHC ->
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHCForm language currentDate

                Nothing ->
                    emptyNode

        call114Form =
            measurements.call114
                |> Maybe.map
                    (Tuple.second
                        >> .value
                    )
                |> call114FormWithDefault data.call114Form

        contactHCTaskDisplayed =
            call114Form.called114 == Just False

        getNextTask currentTask =
            case currentTask of
                NextStepsIsolation ->
                    let
                        tasksList =
                            if contactHCTaskDisplayed then
                                [ NextStepsCall114, NextStepsContactHC ]

                            else
                                [ NextStepsCall114 ]
                    in
                    tasksList
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                NextStepsCall114 ->
                    let
                        tasksList =
                            if contactHCTaskDisplayed then
                                [ NextStepsContactHC ]

                            else
                                [ NextStepsIsolation ]
                    in
                    tasksList
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                NextStepsContactHC ->
                    [ NextStepsIsolation, NextStepsCall114 ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                NextStepsMedicationDistribution ->
                    Nothing

                NextStepsSendToHC ->
                    Nothing

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            nextTask =
                                getNextTask task

                            saveMsg =
                                case task of
                                    NextStepsIsolation ->
                                        SaveIsolation personId measurements.isolation nextTask

                                    NextStepsContactHC ->
                                        SaveHCContact personId measurements.hcContact nextTask

                                    NextStepsCall114 ->
                                        SaveCall114 personId measurements.call114 nextTask

                                    NextStepsSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC

                                    NextStepsMedicationDistribution ->
                                        SaveMedicationDistribution personId measurements.medicationDistribution
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


viewIsolationForm : Language -> NominalDate -> AcuteIllnessMeasurements -> IsolationForm -> Html Msg
viewIsolationForm language currentDate measurements form =
    let
        patientIsolatedInput =
            [ viewQuestionLabel language Translate.PatientIsolatedQuestion
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
                    [ viewQuestionLabel language Translate.SignOnDoorPostedQuestion
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
            [ viewQuestionLabel language Translate.HealthEducationProvidedQuestion
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
        |> div [ class "ui form next-steps isolation" ]


viewHCContactForm : Language -> NominalDate -> AcuteIllnessMeasurements -> HCContactForm -> Html Msg
viewHCContactForm language currentDate measurements form =
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
                        hcRespnonseInput =
                            [ viewQuestionLabel language Translate.HCResponseQuestion
                            , viewCheckBoxSelectCustomInput language
                                [ SendAmbulance, HomeIsolation, ComeToHealthCenter, ChwMonitoring ]
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

        derrivedInputs =
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

                                derrivedSiteInputs =
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
                            recommendation114Input ++ derrivedSiteInputs

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
        ++ derrivedInputs
        |> div [ class "ui form next-steps call-114" ]


viewSendToHCForm : Language -> NominalDate -> SendToHCForm -> Html Msg
viewSendToHCForm language currentDate form =
    div [ class "ui form send-to-hc" ]
        [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
        , div [ class "instructions" ]
            [ viewSendToHCActionLabel language Translate.CompleteHCReferralForm "icon-forms" Nothing
            , viewSendToHCActionLabel language Translate.SendPatientToHC "icon-shuttle" Nothing
            ]
        , viewQuestionLabel language Translate.ReferredPatientToHealthCenterQuestion
        , viewBoolInput
            language
            form.referToHealthCenter
            SetReferToHealthCenter
            "refer-to-hc"
            Nothing
        , viewQuestionLabel language Translate.HandedReferralFormQuestion
        , viewBoolInput
            language
            form.handReferralForm
            SetHandReferralForm
            "hand-referral-form"
            Nothing
        ]


viewSendToHCActionLabel : Language -> TranslationId -> String -> Maybe NominalDate -> Html any
viewSendToHCActionLabel language actionTranslationId iconClass maybeDate =
    div [ class "header" ] <|
        [ i [ class iconClass ] []
        , text <| translate language actionTranslationId
        ]
            ++ renderDatePart language maybeDate
            ++ [ text "." ]


viewMedicationDistributionForm : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> MedicationDistributionForm -> Html Msg
viewMedicationDistributionForm language currentDate person diagnosis form =
    let
        viewAdministeredMedicationQuestion medicineTranslationId =
            div [ class "label" ]
                [ text <|
                    translate language Translate.AdministeredMedicationQuestion
                        ++ " "
                        ++ translate language medicineTranslationId
                        ++ " "
                        ++ translate language Translate.ToThePatient
                        ++ "?"
                ]

        ( instructions, questions ) =
            let
                viewDerivedQuestion medication reasonToSignFunc =
                    let
                        nonAdministrationSigns =
                            form.nonAdministrationSigns |> Maybe.withDefault EverySet.empty

                        currentValue =
                            getCurrentReasonForMedicaitonNonAdministration reasonToSignFunc form
                    in
                    [ viewQuestionLabel language Translate.WhyNot
                    , viewCheckBoxSelectInput language
                        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy ]
                        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                        currentValue
                        (SetMedicationDistributionMedicationNonAdministrationReason currentValue medication)
                        Translate.MedicationNonAdministrationReason
                    ]

                -- When answer for medication administartion is Yes, we clean the reason for not adminsetering the medication.
                updateNonAdministrationSigns medication reasonToSignFunc value form_ =
                    if value == True then
                        form_.nonAdministrationSigns
                            |> Maybe.andThen
                                (\nonAdministrationSigns ->
                                    getCurrentReasonForMedicaitonNonAdministration reasonToSignFunc form_
                                        |> Maybe.map
                                            (\reason ->
                                                Just <| EverySet.remove (nonAdministrationReasonToSign medication reason) nonAdministrationSigns
                                            )
                                        |> Maybe.withDefault (Just nonAdministrationSigns)
                                )

                    else
                        form_.nonAdministrationSigns
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
                    , [ viewAdministeredMedicationQuestion (Translate.MedicationDistributionSign Coartem)
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
                    , [ viewAdministeredMedicationQuestion (Translate.MedicationDistributionSign ORS)
                      , viewBoolInput
                            language
                            form.ors
                            (SetMedicationDistributionBoolInput orsUpdateFunc)
                            "ors-medication"
                            Nothing
                      ]
                        ++ orsDerivedQuestion
                        ++ [ viewAdministeredMedicationQuestion (Translate.MedicationDistributionSign Zinc)
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
                    , [ viewAdministeredMedicationQuestion (Translate.MedicationDistributionSign LemonJuiceOrHoney)
                      , viewBoolInput
                            language
                            form.lemonJuiceOrHoney
                            (SetMedicationDistributionBoolInput lemonJuiceOrHoneyUpdateFunc)
                            "lemon-juice-or-honey-medication"
                            Nothing
                      ]
                    )

                Just DiagnosisRespiratoryInfectionUncomplicated ->
                    let
                        amoxicillinUpdateFunc value form_ =
                            { form_ | amoxicillin = Just value, nonAdministrationSigns = updateNonAdministrationSigns Amoxicillin MedicationAmoxicillin value form_ }

                        derivedQuestion =
                            case form.amoxicillin of
                                Just False ->
                                    viewDerivedQuestion Amoxicillin MedicationAmoxicillin

                                _ ->
                                    []
                    in
                    ( resolveAmoxicillinDosage currentDate person
                        |> Maybe.map
                            (\dosage ->
                                div [ class "instructions respiratory-infection-uncomplicated" ]
                                    [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign Amoxicillin) "icon-pills" Nothing
                                    , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 5)
                                    ]
                            )
                        |> Maybe.withDefault emptyNode
                    , [ viewAdministeredMedicationQuestion (Translate.MedicationDistributionSign Amoxicillin)
                      , viewBoolInput
                            language
                            form.amoxicillin
                            (SetMedicationDistributionBoolInput amoxicillinUpdateFunc)
                            "amoxicillin-medication"
                            Nothing
                      ]
                        ++ derivedQuestion
                    )

                _ ->
                    ( emptyNode, [] )
    in
    div [ class "ui form medication-distribution" ] <|
        [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
        , instructions
        ]
            ++ questions


viewAdministeredMedicationLabel : Language -> TranslationId -> TranslationId -> String -> Maybe NominalDate -> Html any
viewAdministeredMedicationLabel language administerTranslationId medicineTranslationId iconClass maybeDate =
    div [ class "header" ] <|
        [ i [ class iconClass ] []
        , text <| translate language administerTranslationId
        , text " "
        , span [ class "medicine" ] [ text <| translate language medicineTranslationId ]
        ]
            ++ renderDatePart language maybeDate
            ++ [ text ":" ]


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
        [ span [] [ text <| dosage ++ " " ++ translate language Translate.Glass ]
        , text " "
        , text <| translate language Translate.AfterEachLiquidStool
        , text "."
        ]


viewAcuteIllnessOngoingTreatment : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> OngoingTreatmentData -> List (Html Msg)
viewAcuteIllnessOngoingTreatment language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessOngoingTreatment

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
                [ a attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.OngoingTreatmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, ongoingTreatmentTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                OngoingTreatmentReview ->
                    measurements.treatmentOngoing
                        |> Maybe.map (Tuple.second >> .value)
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewOngoingTreatmentReviewForm language currentDate measurements

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


viewOngoingTreatmentReviewForm : Language -> NominalDate -> AcuteIllnessMeasurements -> OngoingTreatmentReviewForm -> Html Msg
viewOngoingTreatmentReviewForm language currentDate measurements form =
    let
        takenAsPrescribedUpdateFunc value form_ =
            if value then
                { form_ | takenAsPrescribed = Just True, reasonForNotTaking = Nothing, reasonForNotTakingDirty = True }

            else
                { form_ | takenAsPrescribed = Just False }

        missedDosesUpdateFunc value form_ =
            if value then
                { form_ | missedDoses = Just True }

            else
                { form_ | missedDoses = Just False, totalMissedDoses = Nothing, totalMissedDosesDirty = True }

        feelingBetterUpdateFunc value form_ =
            { form_ | feelingBetter = Just value }

        sideEffectsUpdateFunc value form_ =
            if value then
                { form_ | sideEffects = Just value }

            else
                { form_ | sideEffects = Just value, adverseEvents = Nothing, adverseEventsDirty = True }

        takenAsPrescribedSection =
            let
                takenAsPrescribed =
                    form.takenAsPrescribed
                        |> Maybe.withDefault True

                reasonForNotTakingInput =
                    if not takenAsPrescribed then
                        [ div [ class "ui grid" ]
                            [ div [ class "one wide column" ] []
                            , div [ class "fifteen wide column" ]
                                [ viewQuestionLabel language Translate.WhyNot ]
                            ]
                        , viewCheckBoxSelectInput language
                            [ NotTakingSideEffects, NotTakingNoResources ]
                            [ NotTakingOther ]
                            form.reasonForNotTaking
                            SetReasonForNotTaking
                            Translate.ReasonForNotTaking
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationTakenAsPrescribedQuestion
            , viewBoolInput
                language
                form.takenAsPrescribed
                (SetOngoingTreatmentReviewBoolInput takenAsPrescribedUpdateFunc)
                "taken-as-prescribed"
                Nothing
            ]
                ++ reasonForNotTakingInput

        missedDosesSection =
            let
                missedDoses =
                    form.missedDoses
                        |> Maybe.withDefault False

                totalMissedDosesInput =
                    if missedDoses then
                        [ div [ class "ui grid" ]
                            [ div [ class "one wide column" ] []
                            , div [ class "four wide column" ]
                                [ viewQuestionLabel language Translate.HowMany ]
                            , div [ class "four wide column" ]
                                [ viewNumberInput language
                                    form.totalMissedDoses
                                    String.fromInt
                                    SetTotalMissedDoses
                                    "total-missed-doses"
                                ]
                            ]
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationDosesMissedQuestion
            , viewBoolInput
                language
                form.missedDoses
                (SetOngoingTreatmentReviewBoolInput missedDosesUpdateFunc)
                "missed-doses"
                Nothing
            ]
                ++ totalMissedDosesInput

        sideEffectsSection =
            let
                sideEffects =
                    form.sideEffects
                        |> Maybe.withDefault False

                adverseEventsInput =
                    if sideEffects then
                        [ div [ class "ui grid" ]
                            [ div [ class "one wide column" ] []
                            , div [ class "fifteen wide column" ]
                                [ viewQuestionLabel language Translate.AcuteIllnessAdverseEventKindsQuestion ]
                            ]
                        , viewCheckBoxMultipleSelectInput language
                            [ AdverseEventRashOrItching
                            , AdverseEventFever
                            , AdverseEventDiarrhea
                            ]
                            [ AdverseEventVomiting
                            , AdverseEventFatigue
                            , AdverseEventOther
                            ]
                            (form.adverseEvents |> Maybe.withDefault [])
                            Nothing
                            SetAdverseEvent
                            Translate.AcuteIllnessAdverseEvent
                        ]

                    else
                        []
            in
            [ viewQuestionLabel language Translate.MedicationCausesSideEffectsQuestion
            , viewBoolInput
                language
                form.sideEffects
                (SetOngoingTreatmentReviewBoolInput sideEffectsUpdateFunc)
                "side-effects"
                Nothing
            ]
                ++ adverseEventsInput
    in
    takenAsPrescribedSection
        ++ missedDosesSection
        ++ [ viewQuestionLabel language Translate.MedicationFeelBetterAfterTakingQuestion
           , viewBoolInput
                language
                form.feelingBetter
                (SetOngoingTreatmentReviewBoolInput feelingBetterUpdateFunc)
                "feeling-better"
                Nothing
           ]
        ++ sideEffectsSection
        |> div [ class "ui form ongoing-treatment-review" ]


viewAcuteIllnessDangerSigns : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> DangerSignsData -> List (Html Msg)
viewAcuteIllnessDangerSigns language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessDangerSigns

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
                [ a attributes
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
                        |> Maybe.map (Tuple.second >> .value)
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
    div [ class "ui form ongoing-treatment-review" ]
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



-- HELPER FUNCTIONS


renderDatePart : Language -> Maybe NominalDate -> List (Html any)
renderDatePart language maybeDate =
    maybeDate
        |> Maybe.map (\date -> [ span [ class "date" ] [ text <| " (" ++ renderDate language date ++ ")" ] ])
        |> Maybe.withDefault []
