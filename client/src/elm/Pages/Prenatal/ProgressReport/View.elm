module Pages.Prenatal.ProgressReport.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model
    exposing
        ( DangerSign(..)
        , EyesCPESign(..)
        , HIVPCRResult(..)
        , HandsCPESign(..)
        , IllnessSymptom(..)
        , MedicationDistributionSign(..)
        , NonReferralSign(..)
        , OutsideCareMedication(..)
        , PrenatalHIVSign(..)
        , PrenatalHealthEducationSign(..)
        , PrenatalMeasurements
        , PrenatalSymptomQuestion(..)
        , ReasonForNonReferral(..)
        , RecommendedTreatmentSign(..)
        , ReferToFacilitySign(..)
        , ReferralFacility(..)
        , SendToHCSign(..)
        , SpecialityCareSign(..)
        , TestExecutionNote(..)
        , TestResult(..)
        , TestVariant(..)
        , ViralLoadStatus(..)
        )
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getMeasurementValueFunc, labExpirationPeriod)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortTuplesByDateDesc)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnosis
        , allObstetricalDiagnosis
        , allRiskFactors
        , allTrimesters
        )
import Backend.PrenatalActivity.Utils
    exposing
        ( generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalProgressReportInitiator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Interval(..), Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils
    exposing
        ( outsideCareMedicationOptionsAnemia
        , outsideCareMedicationOptionsHIV
        , outsideCareMedicationOptionsHypertension
        , outsideCareMedicationOptionsMalaria
        , outsideCareMedicationOptionsSyphilis
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Utils exposing (respiratoryRateElevated)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (viewActionButton)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Prenatal.ProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.Prenatal.ProgressReport.Utils exposing (..)
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.RecurrentEncounter.Utils
import Pages.Prenatal.Utils
    exposing
        ( diagnosedMalaria
        , hypertensionDiagnoses
        , outsideCareDiagnoses
        , outsideCareDiagnosesWithPossibleMedication
        , recommendedTreatmentSignsForHypertension
        , recommendedTreatmentSignsForMalaria
        , recommendedTreatmentSignsForMastitis
        , recommendedTreatmentSignsForSyphilis
        , resolveARVReferralDiagnosis
        , resolveNCDReferralDiagnoses
        )
import Pages.Report.Types exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), TestReport(..))
import Pages.Report.View exposing (..)
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPhotoThumbFromPhotoUrl)
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> PrenatalProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language id initiator model

        content =
            viewWebData language (viewContent language currentDate isChw initiator model) identity assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-report clinical" ] <|
        [ header
        , content
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> PrenatalEncounterId -> PrenatalProgressReportInitiator -> Model -> Html Msg
viewHeader language id initiator model =
    let
        label =
            Maybe.map
                (\mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            Translate.LabResults

                        LabResultsHistory _ ->
                            Translate.LabHistory
                )
                model.labResultsMode
                |> Maybe.withDefault Translate.AntenatalProgressReport

        backIcon =
            let
                iconForView action =
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick action
                            ]
                            []
                        ]

                goBackActionByLabResultsState defaultAction =
                    Maybe.map
                        (\mode ->
                            let
                                backToCurrentMsg targetMode =
                                    SetLabResultsMode (Just (LabResultsCurrent targetMode))
                            in
                            case mode of
                                LabResultsCurrent currentMode ->
                                    case currentMode of
                                        LabResultsCurrentMain ->
                                            SetLabResultsMode Nothing

                                        LabResultsCurrentDipstickShort ->
                                            backToCurrentMsg LabResultsCurrentMain

                                        LabResultsCurrentDipstickLong ->
                                            backToCurrentMsg LabResultsCurrentMain

                                LabResultsHistory historyMode ->
                                    Maybe.withDefault LabResultsCurrentMain model.labResultsHistoryOrigin
                                        |> backToCurrentMsg
                        )
                        model.labResultsMode
                        |> Maybe.withDefault defaultAction
            in
            case initiator of
                InitiatorEncounterPage prenatalEncounterId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId)

                InitiatorRecurrentEncounterPage prenatalEncounterId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage prenatalEncounterId)

                InitiatorNewEncounter _ ->
                    emptyNode

                Backend.PrenatalEncounter.Model.InitiatorPatientRecord patientId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PatientRecordPage InitiatorParticipantDirectory patientId)
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> Bool -> PrenatalProgressReportInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate isChw initiator model assembled =
    let
        derivedContent =
            case model.labResultsMode of
                Just mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            [ viewLabResultsPane language currentDate currentMode assembled ]

                        LabResultsHistory historyMode ->
                            [ viewLabResultsHistoryPane language currentDate historyMode ]

                Nothing ->
                    let
                        firstEncounterMeasurements =
                            getFirstEncounterMeasurements isChw assembled

                        actions =
                            case initiator of
                                InitiatorEncounterPage _ ->
                                    let
                                        ( completedActivities, pendingActivities ) =
                                            getAllActivities assembled
                                                |> List.filter (Pages.Prenatal.Activity.Utils.expectActivity currentDate assembled)
                                                |> List.partition (Pages.Prenatal.Activity.Utils.activityCompleted currentDate assembled)
                                    in
                                    viewActionButton language
                                        pendingActivities
                                        completedActivities
                                        (SetActivePage PinCodePage)
                                        SetEndEncounterDialogState
                                        assembled

                                InitiatorRecurrentEncounterPage _ ->
                                    let
                                        ( completedActivities, pendingActivities ) =
                                            Pages.Prenatal.RecurrentEncounter.Utils.allActivities
                                                |> List.filter (Pages.Prenatal.RecurrentActivity.Utils.expectActivity currentDate assembled)
                                                |> List.partition (Pages.Prenatal.RecurrentActivity.Utils.activityCompleted currentDate assembled)

                                        allowEndEcounter =
                                            List.isEmpty pendingActivities
                                    in
                                    viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState

                                InitiatorNewEncounter encounterId ->
                                    div [ class "actions" ]
                                        [ button
                                            [ class "ui fluid primary button"
                                            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage encounterId
                                            ]
                                            [ text <| translate language Translate.Reviewed ]
                                        ]

                                Backend.PrenatalEncounter.Model.InitiatorPatientRecord _ ->
                                    emptyNode
                    in
                    [ viewRiskFactorsPane language currentDate firstEncounterMeasurements
                    , viewMedicalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
                    , viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
                    , viewChwActivityPane language currentDate isChw assembled
                    , viewPatientProgressPane language currentDate isChw assembled
                    , viewLabsPane language currentDate SetLabResultsMode
                    , viewProgressPhotosPane language currentDate isChw assembled
                    , actions
                    ]
    in
    div [ class "ui unstackable items" ] <|
        viewHeaderPane language currentDate assembled
            :: derivedContent


viewHeaderPane : Language -> NominalDate -> AssembledData -> Html Msg
viewHeaderPane language currentDate assembled =
    let
        mother =
            assembled.person

        ( edd, ega ) =
            assembled.globalLmpDate
                |> generateEDDandEGA language currentDate ( "--/--/----", "----" )

        obstetricHistoryValue =
            assembled.globalObstetricHistory

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value
                    , generatePara value
                    )
                )
                obstetricHistoryValue

        viewLineItem class_ label value =
            p [ class class_ ]
                [ span [ class "label" ] [ text <| translate language label ++ ":" ]
                , span [ class "value" ] [ text value ]
                ]
    in
    div [ class "header-pane" ]
        [ viewItemHeading language Translate.PatientInformation "blue"
        , div [ class "pane-content" ]
            [ div [ class "mother-details" ]
                [ div [ class "ui image" ]
                    [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
                , div [ class "content middle" ]
                    [ p [ class "mother-name" ] [ text mother.name ]
                    , showMaybe <|
                        Maybe.map
                            (\age ->
                                viewLineItem "age-wrapper" Translate.AgeWord (translate language <| Translate.YearsOld age)
                            )
                            (ageInYears currentDate mother)
                    ]
                , div [ class "content right" ]
                    [ viewLineItem "edd" Translate.Edd edd
                    , viewLineItem "ega" Translate.Ega ega
                    ]
                ]
            , div [ class "gravida-para" ]
                [ div [ class "gravida" ]
                    [ div [ class "label" ] [ text <| translate language Translate.Gravida ]
                    , div [ class "value" ] [ text gravida ]
                    ]
                , div [ class "para" ]
                    [ div [ class "label" ] [ text <| translate language Translate.Para ]
                    , div [ class "para-breakdown" ]
                        [ div [ class "term" ]
                            [ div [] [ text <| String.slice 0 1 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.Term ]
                            ]
                        , div [ class "pre-term" ]
                            [ div [] [ text <| String.slice 1 2 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.PreTerm ]
                            ]
                        , div [ class "abortions" ]
                            [ div [] [ text <| String.slice 2 3 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.Abortions ]
                            ]
                        , div [ class "live-children" ]
                            [ div [] [ text <| String.slice 3 4 para ]
                            , div [ class "label small" ] [ text <| translate language Translate.LiveChildren ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewRiskFactorsPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewRiskFactorsPane language currentDate measurements =
    let
        alerts =
            allRiskFactors
                |> List.filterMap (generateRiskFactorAlertData language currentDate measurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.RiskFactors ]
            ]
        , div [ class "pane-content" ] alerts
        ]


viewMedicalDiagnosisPane : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> Html Msg
viewMedicalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled =
    let
        allMeasurementsWithDates =
            assembled.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.encounter.diagnoses, assembled.measurements ) ]
                   )
                |> List.sortWith (sortByDateDesc (\( date, _, _ ) -> date))

        dignoses =
            List.map
                (\( date, diagnoses, measurements ) ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses date diagnoses assembled medicalDiagnoses

                        diagnosesEntries =
                            List.map (viewTreatmentForDiagnosis language date measurements diagnoses) diagnosesIncludingChronic
                                |> List.concat

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis medicalDiagnoses)
                                                >> List.map (viewTreatmentForOutsideCareDiagnosis language date value.medications)
                                                >> List.concat
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        knownAsPositiveEntries =
                            viewKnownPositives language date measurements

                        programReferralEntries =
                            getMeasurementValueFunc measurements.specialityCare
                                |> Maybe.map
                                    (\value ->
                                        let
                                            arvEntry =
                                                resolveARVReferralDiagnosis assembled.nursePreviousMeasurementsWithDates
                                                    |> Maybe.map
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language date diagnosis FacilityARVProgram

                                                            else
                                                                []
                                                        )
                                                    |> Maybe.withDefault []

                                            ncdEntries =
                                                resolveNCDReferralDiagnoses assembled.nursePreviousMeasurementsWithDates
                                                    |> List.map
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language date diagnosis FacilityNCDProgram

                                                            else
                                                                []
                                                        )
                                                    |> List.concat
                                        in
                                        arvEntry ++ ncdEntries
                                    )
                                |> Maybe.withDefault []
                    in
                    knownAsPositiveEntries
                        ++ diagnosesEntries
                        ++ outsideCareDiagnosesEntries
                        ++ programReferralEntries
                )
                allMeasurementsWithDates
                |> List.concat
                |> ul []

        alerts =
            allMedicalDiagnosis
                |> List.filterMap (generateMedicalDiagnosisAlertData language currentDate firstEncounterMeasurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "medical-diagnosis" ]
        [ viewItemHeading language Translate.MedicalDiagnosis "blue"
        , div [ class "pane-content" ] <|
            dignoses
                :: alerts
        ]


viewProgramReferralEntry : Language -> NominalDate -> PrenatalDiagnosis -> ReferralFacility -> List (Html Msg)
viewProgramReferralEntry language date diagnosis facility =
    diagnosisForProgressReportToString language diagnosis
        ++ " - "
        ++ (translate language <| Translate.ReferredToFacilityPostpartum facility)
        ++ " "
        ++ (String.toLower <| translate language Translate.On)
        ++ " "
        ++ formatDDMMYYYY date
        |> wrapWithLI


viewObstetricalDiagnosisPane : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> Html Msg
viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled =
    let
        allMeasurementsWithDates =
            assembled.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.encounter.diagnoses, assembled.measurements ) ]
                   )
                |> List.sortWith (sortByDateDesc (\( date, _, _ ) -> date))

        initialHealthEducationOccurances =
            List.foldr
                (\( date, _, measurements ) accum ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> Maybe.map
                            (\value ->
                                let
                                    signRecord sign =
                                        if
                                            EverySet.member sign value.signs
                                                && (isNothing <| Dict.get sign accum)
                                        then
                                            Just ( sign, date )

                                        else
                                            Nothing
                                in
                                [ signRecord EducationNauseaVomiting
                                , signRecord EducationLegCramps
                                , signRecord EducationLowBackPain
                                , signRecord EducationConstipation
                                , signRecord EducationVaricoseVeins
                                , signRecord EducationLegPainRedness
                                , signRecord EducationPelvicPain
                                ]
                                    |> Maybe.Extra.values
                                    |> Dict.fromList
                                    |> Dict.union accum
                            )
                        |> Maybe.withDefault accum
                )
                Dict.empty
                allMeasurementsWithDates

        dignoses =
            List.map
                (\( date, diagnoses, measurements ) ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses date diagnoses assembled obstetricalDiagnoses

                        diagnosesEntries =
                            List.map (viewTreatmentForDiagnosis language date measurements diagnoses) diagnosesIncludingChronic
                                |> List.concat

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                                                >> List.map (viewTreatmentForOutsideCareDiagnosis language date value.medications)
                                                >> List.concat
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        healthEducationDiagnosesEntries =
                            getMeasurementValueFunc measurements.healthEducation
                                |> Maybe.map
                                    (\value ->
                                        let
                                            formatedDate =
                                                formatDDMMYYYY date

                                            messageForSign sign =
                                                if EverySet.member sign value.signs then
                                                    Dict.get sign initialHealthEducationOccurances
                                                        |> Maybe.map
                                                            (\initialDate ->
                                                                let
                                                                    currentIsInitial =
                                                                        Date.compare initialDate date == EQ
                                                                in
                                                                Translate.PrenatalHealthEducationSignsDiagnosis currentIsInitial formatedDate sign
                                                                    |> translate language
                                                                    |> wrapWithLI
                                                            )

                                                else
                                                    Nothing
                                        in
                                        [ messageForSign EducationNauseaVomiting
                                        , messageForSign EducationLegCramps
                                        , messageForSign EducationLowBackPain
                                        , messageForSign EducationConstipation
                                        , messageForSign EducationVaricoseVeins
                                        , messageForSign EducationLegPainRedness
                                        , messageForSign EducationPelvicPain
                                        ]
                                            |> Maybe.Extra.values
                                            |> List.concat
                                    )
                                |> Maybe.withDefault []
                    in
                    diagnosesEntries ++ outsideCareDiagnosesEntries ++ healthEducationDiagnosesEntries
                )
                allMeasurementsWithDates
                |> List.concat
                |> ul []

        alerts =
            allObstetricalDiagnosis
                |> List.filterMap (generateObstetricalDiagnosisAlertData language currentDate isChw firstEncounterMeasurements assembled)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "obstetric-diagnosis" ]
        [ viewItemHeading language Translate.ObstetricalDiagnosis "blue"
        , div [ class "pane-content" ] <|
            dignoses
                :: alerts
        ]


viewChwActivityPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewChwActivityPane language currentDate isChw assembled =
    let
        allMeasurementsWithDates =
            assembled.chwPreviousMeasurementsWithDates
                ++ (if isChw then
                        [ ( currentDate, assembled.encounter.encounterType, assembled.measurements ) ]

                    else
                        []
                   )
                |> List.sortWith (sortByDateDesc (\( date, _, _ ) -> date))

        activitiesWithDate =
            List.map
                (\( date, _, measurements ) ->
                    ( date, List.filter (matchCHWActivityAtEncounter measurements) allCHWActions )
                )
                allMeasurementsWithDates

        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ text <| translate language Translate.Date ]
                , div [ class "chw-actions" ] [ text <| translate language Translate.Actions ]
                ]

        actions =
            List.map
                (\( date, activities ) ->
                    div [ class "table-row" ]
                        [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                        , List.map
                            (\activity ->
                                li [ class <| chwActionToColor activity ]
                                    [ text <| translate language <| Translate.CHWAction activity ]
                            )
                            activities
                            |> ul [ class "chw-actions" ]
                        ]
                )
                activitiesWithDate
    in
    div [ class "chw-activities" ]
        [ viewItemHeading language Translate.ChwActivity "blue"
        , div [ class "pane-content" ] <|
            heading
                :: actions
        ]


matchCHWActivityAtEncounter : PrenatalMeasurements -> CHWAction -> Bool
matchCHWActivityAtEncounter measurements activity =
    case activity of
        ActionPregnancyDating ->
            isJust measurements.lastMenstrualPeriod

        ActionLabs ->
            isJust measurements.pregnancyTest

        ActionDangerSignsPresent ->
            getMeasurementValueFunc measurements.dangerSigns
                |> Maybe.map
                    (\value ->
                        case EverySet.toList value.signs of
                            [] ->
                                False

                            [ NoDangerSign ] ->
                                False

                            _ ->
                                True
                    )
                |> Maybe.withDefault False

        ActionReferredToHealthCenter ->
            getMeasurementValueFunc measurements.sendToHC
                |> Maybe.andThen (.sendToHCSigns >> Maybe.map (EverySet.member ReferToHealthCenter))
                |> Maybe.withDefault False

        ActionAppointmentConfirmation ->
            isJust measurements.appointmentConfirmation

        ActionHealthEducation ->
            isJust measurements.healthEducation

        ActionBirthPlan ->
            isJust measurements.birthPlan


viewPatientProgressPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewPatientProgressPane language currentDate isChw assembled =
    let
        allMeasurementsWithDates =
            List.map (\( date, _, measurements ) -> ( date, measurements )) assembled.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.measurements ) ]
                   )

        allMeasurements =
            allMeasurementsWithDates
                |> List.map Tuple.second

        encountersTrimestersData =
            allMeasurementsWithDates
                |> List.map
                    (\( date, _ ) ->
                        ( date
                        , getEncounterTrimesterData date assembled.globalLmpDate
                        )
                    )

        getTrimesterEncounters trimester =
            encountersTrimestersData
                |> List.filter (\t -> Tuple.second t == Just trimester)
                |> List.map Tuple.first

        encountersFirstTrimester =
            getTrimesterEncounters FirstTrimester

        encountersFirstTrimesterCount =
            List.length encountersFirstTrimester

        encountersSecondTrimester =
            getTrimesterEncounters SecondTrimester

        encountersSecondTrimesterCount =
            List.length encountersSecondTrimester

        encountersThirdTrimester =
            getTrimesterEncounters ThirdTrimester

        encountersThirdTrimesterCount =
            List.length encountersThirdTrimester

        fetalMovementsDate =
            allMeasurementsWithDates
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalMovement >> (==) True)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        fetalHeartRateDate =
            allMeasurementsWithDates
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalHeartRate >> (<) 0)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        egaWeeksDaysLabel language_ encounterDate lmpDate =
            let
                diffInDays =
                    diffDays lmpDate encounterDate
            in
            generateEGAWeeksDaysLabel language_ diffInDays

        ( eddLabel, fetalHeartRateLabel, fetalMovementsLabel ) =
            assembled.globalLmpDate
                |> Maybe.map
                    (\lmpDate ->
                        let
                            eddDate =
                                lmpToEDDDate lmpDate
                        in
                        ( div [ class "due-date-label" ]
                            [ div [] [ text <| translate language Translate.DueDate ++ ":" ]
                            , div []
                                [ text <|
                                    (Date.day eddDate |> String.fromInt)
                                        ++ " "
                                        ++ translate language (Translate.ResolveMonth False (Date.month eddDate))
                                ]
                            ]
                        , fetalHeartRateDate
                            |> Maybe.map
                                (\date ->
                                    div [ class "heart-rate-label" ]
                                        [ span [] [ text <| translate language Translate.FetalHeartRate ++ ": " ]
                                        , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode
                        , fetalMovementsDate
                            |> Maybe.map
                                (\date ->
                                    div [ class "movements-label" ]
                                        [ span [] [ text <| translate language Translate.FetalMovement ++ ": " ]
                                        , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode
                        )
                    )
                |> Maybe.withDefault ( emptyNode, emptyNode, emptyNode )

        viewTrimesterTimeline trimester =
            let
                encounterIconWidth =
                    18

                currentEncounterTrimester =
                    if encountersThirdTrimesterCount > 0 then
                        ThirdTrimester

                    else if encountersSecondTrimesterCount > 0 then
                        SecondTrimester

                    else
                        FirstTrimester

                periodWidth =
                    case trimester of
                        FirstTrimester ->
                            (180 - encounterIconWidth * encountersFirstTrimesterCount) // (encountersFirstTrimesterCount + 1)

                        SecondTrimester ->
                            (180 - encounterIconWidth * encountersSecondTrimesterCount) // (encountersSecondTrimesterCount + 1)

                        ThirdTrimester ->
                            (210 - encounterIconWidth * encountersThirdTrimesterCount) // (encountersThirdTrimesterCount + 1)

                ( trimesterPeriodsColors, trimesterEncountersDates ) =
                    case trimester of
                        FirstTrimester ->
                            ( if currentEncounterTrimester == FirstTrimester then
                                List.repeat encountersFirstTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersFirstTrimesterCount + 1) "blue"
                            , encountersFirstTrimester
                            )

                        SecondTrimester ->
                            ( if currentEncounterTrimester == SecondTrimester then
                                List.repeat encountersSecondTrimesterCount "blue" ++ [ "gray" ]

                              else if currentEncounterTrimester == FirstTrimester then
                                List.repeat (encountersSecondTrimesterCount + 1) "gray"

                              else
                                List.repeat (encountersSecondTrimesterCount + 1) "blue"
                            , encountersSecondTrimester
                            )

                        ThirdTrimester ->
                            ( if currentEncounterTrimester == ThirdTrimester then
                                List.repeat encountersThirdTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersThirdTrimesterCount + 1) "gray"
                            , encountersThirdTrimester
                            )

                fetalMovementsIcon =
                    span [ class "fetal-movements" ]
                        [ img
                            [ src "assets/images/icon-fetal-movement.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                fetalHeartRateIcon rightMargin =
                    span
                        [ class "fetal-heart-rate"
                        , style "margin-right" rightMargin
                        ]
                        [ img
                            [ src "assets/images/icon-fetal-heartrate.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                dueDateInfo =
                    if trimester == ThirdTrimester then
                        div [ class "due-date-info" ]
                            [ span [ class "due-date-icon" ] [ img [ src "assets/images/icon-baby-due-date.png" ] [] ]
                            , eddLabel
                            ]

                    else
                        emptyNode

                trimesterPeriods =
                    trimesterPeriodsColors
                        |> List.map
                            (\color ->
                                p
                                    [ class <| "period " ++ color
                                    , style "width" (String.fromInt periodWidth ++ "px")
                                    ]
                                    []
                            )

                timelineIcons date =
                    if fetalMovementsDate == Just date && fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-25px", style "width" "65px" ]
                            [ fetalHeartRateIcon "5px"
                            , fetalMovementsIcon
                            ]

                    else if fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-6px", style "width" "35px" ] [ fetalHeartRateIcon "0" ]

                    else if fetalMovementsDate == Just date then
                        div [ style "margin-left" "-2px", style "width" "30px" ] [ fetalMovementsIcon ]

                    else
                        emptyNode

                trimesterEncounters =
                    trimesterEncountersDates
                        |> List.map
                            (\date ->
                                span [ style "width" (String.fromInt encounterIconWidth ++ "px") ]
                                    [ img
                                        [ src "assets/images/icon-blue-circle.png"
                                        , style "width" (String.fromInt encounterIconWidth ++ "px")
                                        ]
                                        []
                                    , timelineIcons date
                                    ]
                            )
            in
            List.Extra.interweave trimesterPeriods trimesterEncounters
                |> List.append [ dueDateInfo ]
                |> div [ class "trimester-timeline" ]

        viewTrimesterVisits trimester =
            let
                ( expectedVisits, actualVisists, visitsLabel ) =
                    case trimester of
                        FirstTrimester ->
                            ( 1, encountersFirstTrimesterCount, Translate.OneVisit )

                        SecondTrimester ->
                            ( 2, encountersSecondTrimesterCount, Translate.TwoVisits )

                        ThirdTrimester ->
                            ( 5, encountersThirdTrimesterCount, Translate.FiveVisits )

                actualVisists_ =
                    if actualVisists > expectedVisits then
                        expectedVisits

                    else
                        actualVisists

                missingVisits =
                    expectedVisits - actualVisists_

                visitsView =
                    List.repeat actualVisists_ "icon-checked-green-circle.png"
                        ++ List.repeat missingVisits "icon-gray-circle-small.png"
                        |> List.map (\icon -> img [ src <| "assets/images/" ++ icon ] [])
            in
            div [ class "trimester-visits" ]
                [ div [ class "label-trimester" ] [ text <| translate language <| Translate.PregnancyTrimester trimester ]
                , div [ class "details" ]
                    [ div [ class "label-visit" ] [ text <| translate language visitsLabel ]
                    , div [ class "visits" ] visitsView
                    ]
                ]

        viewChartHeading transId =
            div [ class "chart-heading" ]
                [ img [ src <| "assets/images/icon-gray-circle-small.png" ] []
                , span [] [ text <| translate language transId ]
                ]

        egaBmiValues =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        assembled.globalLmpDate
                            |> Maybe.map
                                (\lmpDate ->
                                    let
                                        bmi =
                                            measurements.nutrition
                                                |> Maybe.map
                                                    (\measurement ->
                                                        let
                                                            height =
                                                                Tuple.second measurement
                                                                    |> .value
                                                                    |> .height
                                                                    |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)

                                                            weight =
                                                                Tuple.second measurement
                                                                    |> .value
                                                                    |> .weight
                                                                    |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                                        in
                                                        calculateBmi (Just height) (Just weight)
                                                            |> Maybe.withDefault 0
                                                    )
                                                |> Maybe.withDefault 0
                                    in
                                    ( diffDays lmpDate date, bmi )
                                )
                    )

        egaFundalHeightValues =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        assembled.globalLmpDate
                            |> Maybe.map
                                (\lmpDate ->
                                    let
                                        fundalHeight =
                                            measurements.obstetricalExam
                                                |> Maybe.map
                                                    (\measurement ->
                                                        Tuple.second measurement
                                                            |> .value
                                                            |> .fundalHeight
                                                            |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                                    )
                                                |> Maybe.withDefault 0
                                    in
                                    ( diffDays lmpDate date, fundalHeight )
                                )
                    )
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ div [ class "caption timeline" ] [ text <| translate language Translate.ProgressTimeline ++ ":" ]
            , div [ class "timeline-section" ]
                [ div [ class "indicators" ]
                    [ fetalHeartRateLabel
                    , fetalMovementsLabel
                    ]
                , allTrimesters
                    |> List.map viewTrimesterTimeline
                    |> div [ class "timeline" ]
                ]
            , allTrimesters
                |> List.map viewTrimesterVisits
                |> div [ class "visits-section" ]
            , div [ class "caption trends" ] [ text <| translate language Translate.ProgressTrends ++ ":" ]
            , div [ class "trends-section" ]
                [ viewMarkers
                , div [ class "bmi-info" ]
                    [ viewChartHeading Translate.BMI
                    , heightWeightBMITable language currentDate assembled.globalLmpDate allMeasurementsWithDates
                    , viewBMIForEGA language egaBmiValues
                    , illustrativePurposes language
                    ]
                , div [ class "fundal-height-info" ]
                    [ viewChartHeading Translate.FundalHeight
                    , fundalHeightTable language currentDate assembled.globalLmpDate allMeasurementsWithDates
                    , viewFundalHeightForEGA language egaFundalHeightValues
                    , illustrativePurposes language
                    ]
                ]
            ]
        ]


tableEgaHeading : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
tableEgaHeading language currentDate maybeLmpDate measurementsWithDates =
    measurementsWithDates
        |> List.map
            (\( date, measurements ) ->
                maybeLmpDate
                    |> Maybe.map
                        (\lmpDate ->
                            diffDays lmpDate date
                                |> generateEGAWeeksDaysLabel language
                                |> String.toLower
                                |> text
                                |> List.singleton
                        )
                    |> Maybe.withDefault [ text "--" ]
                    |> th
                        [ classList
                            [ ( "center", True )
                            , ( "bottom", True )
                            , ( "aligned", True )
                            , ( "ega-header", True )
                            ]
                        ]
            )
        |> (::)
            (th
                [ class "uppercase" ]
                [ text <| translate language Translate.Ega ]
            )
        |> tr []


heightWeightBMITable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
heightWeightBMITable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| String.fromFloat height ++ translate language Translate.CentimeterShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Height)
                            |> tr []

                    weights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                            in
                                            [ text <| String.fromFloat weight ++ translate language Translate.KilogramShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Weight)
                            |> tr []

                    bmis =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)

                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)

                                                bmi =
                                                    calculateBmi (Just height) (Just weight)
                                                        |> Maybe.withDefault 0
                                                        |> Round.round 1
                                            in
                                            [ text bmi ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.BMI)
                            |> tr []
                in
                [ egas
                , heights
                , weights
                , bmis
                ]
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


fundalHeightTable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
fundalHeightTable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .obstetricalExam
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .fundalHeight
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| String.fromFloat height ++ translate language Translate.CentimeterShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.FundalHeight)
                            |> tr []
                in
                [ egas
                , heights
                ]
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


illustrativePurposes : Language -> Html any
illustrativePurposes language =
    div [ class "illustrative-purposes" ] [ text <| translate language Translate.ForIllustrativePurposesOnly ]


viewLabResultsPane : Language -> NominalDate -> LabResultsCurrentMode -> AssembledData -> Html Msg
viewLabResultsPane language currentDate mode assembled =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "name" ] [ translateText language Translate.TestName ]
                , div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                , div [ class "normal-range" ] [ translateText language Translate.NormalRange ]
                ]

        measurementsWithLabResults =
            assembled.measurements
                :: List.map (\( _, _, measurements ) -> measurements) assembled.nursePreviousMeasurementsWithDates

        getTestResults getMeasurementFunc getResultFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                measurementsWithLabResults
                |> List.filterMap
                    (\value ->
                        if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        getTestResultsKnownAsPositive getMeasurementFunc getResultFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                measurementsWithLabResults
                |> List.filterMap
                    (\value ->
                        if value.executionNote == TestNoteKnownAsPositive then
                            Just ( currentDate, Just TestNotPerformedKnownAsPositive )

                        else if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value |> Maybe.map TestPerformed ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        hivTestResults =
            getTestResultsKnownAsPositive .hivTest .testResult

        hivPCRTestResults =
            getTestResults .hivPCRTest
                (\value ->
                    Maybe.andThen
                        (\status ->
                            case status of
                                ViralLoadUndetectable ->
                                    Just ResultSuppressedViralLoad

                                ViralLoadDetectable ->
                                    Maybe.map ResultDetectibleViralLoad value.hivViralLoad
                        )
                        value.hivViralLoadStatus
                )

        syphilisTestResults =
            getTestResults .syphilisTest .testResult

        hepatitisBTestResults =
            getTestResultsKnownAsPositive .hepatitisBTest .testResult

        malariaTestResults =
            getTestResults .malariaTest .testResult

        urineDipstickTestValues =
            List.filterMap (.urineDipstickTest >> getMeasurementValueFunc)
                measurementsWithLabResults

        urineDipstickTestResults =
            List.filterMap
                (\value ->
                    if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                        Maybe.map (\executionDate -> ( executionDate, ( value.protein, value.ph, value.glucose ) ))
                            value.executionDate

                    else
                        Nothing
                )
                urineDipstickTestValues
                |> List.sortWith sortTuplesByDateDesc

        longUrineDipstickTestResults =
            List.filterMap
                (\value ->
                    if value.testVariant == Just VariantLongTest && List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                        Maybe.map (\executionDate -> ( executionDate, value ))
                            value.executionDate

                    else
                        Nothing
                )
                urineDipstickTestValues
                |> List.sortWith sortTuplesByDateDesc

        proteinResults =
            List.map (\( date, ( protein, _, _ ) ) -> ( date, protein )) urineDipstickTestResults

        phResults =
            List.map (\( date, ( _, ph, _ ) ) -> ( date, ph )) urineDipstickTestResults

        glucoseResults =
            List.map (\( date, ( _, _, glucose ) ) -> ( date, glucose )) urineDipstickTestResults

        leukocytesResults =
            List.map (\( date, value ) -> ( date, value.leukocytes )) longUrineDipstickTestResults

        nitriteResults =
            List.map (\( date, value ) -> ( date, value.nitrite )) longUrineDipstickTestResults

        urobilinogenResults =
            List.map (\( date, value ) -> ( date, value.urobilinogen )) longUrineDipstickTestResults

        haemoglobinResults =
            List.map (\( date, value ) -> ( date, value.haemoglobin )) longUrineDipstickTestResults

        ketoneResults =
            List.map (\( date, value ) -> ( date, value.ketone )) longUrineDipstickTestResults

        bilirubinResults =
            List.map (\( date, value ) -> ( date, value.bilirubin )) longUrineDipstickTestResults

        randomBloodSugarResults =
            getTestResults .randomBloodSugarTest .sugarCount

        hemoglobinResults =
            getTestResults .hemoglobinTest .hemoglobinCount

        bloodGpRsResults =
            getTestResults .bloodGpRsTest (\value -> ( value.bloodGroup, value.rhesus ))

        bloodGroupResults =
            List.map (\( date, ( bloodGroup, _ ) ) -> ( date, bloodGroup )) bloodGpRsResults

        rhesusResults =
            List.map (\( date, ( _, rhesus ) ) -> ( date, rhesus )) bloodGpRsResults

        content =
            case mode of
                LabResultsCurrentMain ->
                    [ viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHIV hivTestResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHIVPCR hivPCRTestResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistorySyphilis syphilisTestResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHepatitisB hepatitisBTestResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryMalaria malariaTestResults)
                    , dipstickShortEntry
                    , dipstickLongEntry
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHemoglobin hemoglobinResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryBloodGroup bloodGroupResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryRhesus rhesusResults)
                    ]

                LabResultsCurrentDipstickShort ->
                    [ proteinEntry
                    , phEntry
                    , glucoseEntry
                    ]

                LabResultsCurrentDipstickLong ->
                    [ proteinEntry
                    , phEntry
                    , glucoseEntry
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryLeukocytes leukocytesResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryNitrite nitriteResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryUrobilinogen urobilinogenResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHaemoglobin haemoglobinResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryKetone ketoneResults)
                    , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryBilirubin bilirubinResults)
                    ]

        proteinEntry =
            viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryProtein proteinResults)

        phEntry =
            viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryPH phResults)

        glucoseEntry =
            viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryGlucose glucoseResults)

        dipstickShortEntry =
            List.head proteinResults
                |> Maybe.map
                    (\( date, proteinResult ) ->
                        let
                            proteinResultNormal_ =
                                Maybe.map proteinResultNormal proteinResult
                                    |> -- It's ok not to have a result, because test
                                       -- may have been not performed yet.
                                       Maybe.withDefault True

                            resultsNormal =
                                if not proteinResultNormal_ then
                                    False

                                else
                                    Maybe.map2
                                        (\phResult glucoseResult ->
                                            phResultNormal phResult && glucoseResultNormal glucoseResult
                                        )
                                        (List.head phResults |> Maybe.andThen Tuple.second)
                                        (List.head glucoseResults |> Maybe.andThen Tuple.second)
                                        |> -- We should never get here since protein result
                                           -- existed, and all 3 are entered together.
                                           Maybe.withDefault False

                            result =
                                if resultsNormal then
                                    translate language Translate.Normal

                                else
                                    translate language Translate.Abnormal
                        in
                        viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest)
                            (formatDDMMYYYY date)
                            result
                            (Just <| SetLabResultsMode <| Just <| LabResultsCurrent LabResultsCurrentDipstickShort)
                            resultsNormal
                    )
                |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest))

        dipstickLongEntry =
            List.head leukocytesResults
                |> Maybe.map
                    (\( date, leukocytesResult ) ->
                        let
                            leukocytesResultNormal_ =
                                Maybe.map leukocytesResultNormal leukocytesResult
                                    |> -- It's ok not to have a result, because test
                                       -- may have been not performed yet.
                                       Maybe.withDefault True

                            resultsNormal =
                                if not leukocytesResultNormal_ then
                                    False

                                else
                                    let
                                        firstGroupResultsNormal =
                                            Maybe.map5
                                                (\proteinResult phResult glucoseResult nitriteResult urobilinogenResult ->
                                                    proteinResultNormal proteinResult
                                                        && phResultNormal phResult
                                                        && glucoseResultNormal glucoseResult
                                                        && nitriteResultNormal nitriteResult
                                                        && urobilinogenResultNormal urobilinogenResult
                                                )
                                                (List.head proteinResults |> Maybe.andThen Tuple.second)
                                                (List.head phResults |> Maybe.andThen Tuple.second)
                                                (List.head glucoseResults |> Maybe.andThen Tuple.second)
                                                (List.head nitriteResults |> Maybe.andThen Tuple.second)
                                                (List.head urobilinogenResults |> Maybe.andThen Tuple.second)
                                                |> -- We should never get here since leukocytes result
                                                   -- existed, and all the results are entered together.
                                                   Maybe.withDefault False

                                        secondGroupResultsNormal =
                                            Maybe.map3
                                                (\haemoglobinResult ketoneResult bilirubinResult ->
                                                    urineHaemoglobinValueResultNormal haemoglobinResult
                                                        && ketoneResultNormal ketoneResult
                                                        && bilirubinResultNormal bilirubinResult
                                                )
                                                (List.head haemoglobinResults |> Maybe.andThen Tuple.second)
                                                (List.head ketoneResults |> Maybe.andThen Tuple.second)
                                                (List.head bilirubinResults |> Maybe.andThen Tuple.second)
                                                |> -- We should never get here since leukocytes result
                                                   -- existed, and all the results are entered together.
                                                   Maybe.withDefault False
                                    in
                                    firstGroupResultsNormal && secondGroupResultsNormal

                            result =
                                if resultsNormal then
                                    translate language Translate.Normal

                                else
                                    translate language Translate.Abnormal
                        in
                        viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest)
                            (formatDDMMYYYY date)
                            result
                            (Just <| SetLabResultsMode <| Just <| LabResultsCurrent LabResultsCurrentDipstickLong)
                            resultsNormal
                    )
                |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest))

        emptyDipstickEntry label =
            viewDipstickEntry label "--/--/----" "---" Nothing True

        viewDipstickEntry label date result maybeAction resultNormal =
            let
                forwardIcon =
                    Maybe.map
                        (\action ->
                            div
                                [ class "icon-forward"
                                , onClick action
                                ]
                                []
                        )
                        maybeAction
                        |> Maybe.withDefault emptyNode
            in
            div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
                [ div [ class "name" ] [ translateText language label ]
                , div [ class "date" ] [ text date ]
                , div [ class "result" ] [ text result ]
                , div [ class "normal-range" ] [ translateText language Translate.Normal ]
                , forwardIcon
                ]
    in
    div [ class "lab-results" ]
        [ viewItemHeading language (Translate.LabResultsPaneHeader mode) "blue"
        , div [ class "pane-content" ] [ heading ]
        , div [ class "group-content" ] content
        ]


viewProgressPhotosPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewProgressPhotosPane language currentDate isChw assembled =
    let
        allMeasurementsWithDates =
            List.map (\( date, _, measurements ) -> ( date, measurements )) assembled.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.measurements ) ]
                   )

        content =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        measurements.prenatalPhoto
                            |> Maybe.map
                                (Tuple.second
                                    >> .value
                                    >> (\photoUrl ->
                                            let
                                                egaLabel =
                                                    assembled.globalLmpDate
                                                        |> Maybe.map (\lmpDate -> diffDays lmpDate date |> generateEGAWeeksDaysLabel language)
                                                        |> Maybe.withDefault ""
                                            in
                                            div [ class "progress-photo" ]
                                                [ viewPhotoThumbFromPhotoUrl photoUrl
                                                , div [ class "ega" ] [ text egaLabel ]
                                                ]
                                       )
                                )
                    )
    in
    div [ class "progress-photos" ]
        [ viewItemHeading language Translate.ProgressPhotos "blue"
        , div [ class "pane-content" ] content
        ]


viewKnownPositives :
    Language
    -> NominalDate
    -> PrenatalMeasurements
    -> List (Html any)
viewKnownPositives language date measurements =
    let
        resolveKnownPositive getMeasurementFunc knownPositiveTransId =
            getMeasurementFunc measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        if value.executionNote == TestNoteKnownAsPositive then
                            li []
                                [ text <|
                                    translate language knownPositiveTransId
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                ]

                        else
                            emptyNode
                    )
                |> Maybe.withDefault emptyNode
    in
    [ resolveKnownPositive .hivTest Translate.KnownPositiveHIV
    , resolveKnownPositive .hepatitisBTest Translate.KnownPositiveHepatitisB
    ]


viewTreatmentForDiagnosis :
    Language
    -> NominalDate
    -> PrenatalMeasurements
    -> EverySet PrenatalDiagnosis
    -> PrenatalDiagnosis
    -> List (Html any)
viewTreatmentForDiagnosis language date measurements allDiagnoses diagnosis =
    let
        diagnosisForProgressReport =
            diagnosisForProgressReportToString language diagnosis

        hypertensionTreatmentMessage =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                        >> List.head
                        >> Maybe.map
                            (\treatmentSign ->
                                if treatmentSign == NoTreatmentForHypertension then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        continued =
                                            if EverySet.member diagnosis allDiagnoses then
                                                ""

                                            else
                                                " (" ++ (String.toLower <| translate language Translate.Continued) ++ ") "

                                        treatmentLabel =
                                            case treatmentSign of
                                                TreatmentHypertensionAddCarvedilol ->
                                                    [ TreatmentMethyldopa4, TreatmentHypertensionAddCarvedilol ]
                                                        |> List.map (Translate.RecommendedTreatmentSignLabel >> translate language)
                                                        |> String.join ", "

                                                TreatmentHypertensionAddAmlodipine ->
                                                    [ TreatmentMethyldopa4, TreatmentHypertensionAddCarvedilol, TreatmentHypertensionAddAmlodipine ]
                                                        |> List.map (Translate.RecommendedTreatmentSignLabel >> translate language)
                                                        |> String.join ", "

                                                _ ->
                                                    translate language <| Translate.RecommendedTreatmentSignLabel treatmentSign
                                    in
                                    diagnosisForProgressReport
                                        ++ continued
                                        ++ " - "
                                        ++ (String.toLower <| translate language Translate.TreatedWith)
                                        ++ " "
                                        ++ treatmentLabel
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                            )
                        >> Maybe.withDefault noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        syphilisTreatmentMessage complications =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForSyphilis)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForSyphilis treatment then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabel medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ complications
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        malariaTreatmentMessage =
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForMalaria)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForMalaria treatment then
                                    noTreatmentAdministeredMessage

                                else if List.member TreatmentReferToHospital treatment then
                                    referredToHospitalMessage

                                else if List.member TreatmentWrittenProtocols treatment then
                                    translate language Translate.MalariaWithGIComplications
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        ++ " - "
                                        ++ translate language Translate.WrittenProtocolsFollowed
                                        |> wrapWithLI

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabel medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        mentalHealthMessage =
            translate language Translate.EPDSPreformedOn
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ diagnosisForProgressReport
                |> wrapWithLI

        referredToHospitalMessage =
            referredToHospitalMessageWithComplications ""

        referredToHospitalMessageWithComplications complications =
            referredToFacilityMessageWithComplications FacilityHospital complications

        referredToFacilityMessage facility =
            referredToFacilityMessageWithComplications facility ""

        referredToFacilityMessageWithComplications facility complications =
            if isNothing measurements.sendToHC then
                noTreatmentRecordedMessageWithComplications complications

            else
                let
                    refferedToFacility =
                        getMeasurementValueFunc measurements.sendToHC
                            |> Maybe.andThen .referToFacilitySigns
                            |> Maybe.map
                                (\referToFacilitySigns ->
                                    case facility of
                                        FacilityHospital ->
                                            EverySet.member ReferToHospital referToFacilitySigns

                                        FacilityMentalHealthSpecialist ->
                                            EverySet.member ReferToMentalHealthSpecialist referToFacilitySigns

                                        FacilityARVProgram ->
                                            EverySet.member ReferToARVProgram referToFacilitySigns

                                        FacilityNCDProgram ->
                                            EverySet.member ReferToNCDProgram referToFacilitySigns

                                        FacilityANCServices ->
                                            -- Explicit NCD facility.
                                            False

                                        FacilityHealthCenter ->
                                            -- We should never get here.
                                            False
                                )
                            |> Maybe.withDefault False
                in
                if refferedToFacility then
                    diagnosisForProgressReport
                        ++ complications
                        ++ " - "
                        ++ (String.toLower <| translate language <| Translate.ReferredToFacility facility)
                        ++ " "
                        ++ (String.toLower <| translate language Translate.On)
                        ++ " "
                        ++ formatDDMMYYYY date
                        |> wrapWithLI

                else
                    let
                        reason =
                            getMeasurementValueFunc measurements.sendToHC
                                |> Maybe.andThen
                                    (\value ->
                                        case facility of
                                            FacilityHospital ->
                                                getCurrentReasonForNonReferral NonReferralReasonHospital value.facilityNonReferralReasons

                                            FacilityMentalHealthSpecialist ->
                                                getCurrentReasonForNonReferral NonReferralReasonMentalHealthSpecialist value.facilityNonReferralReasons

                                            FacilityARVProgram ->
                                                getCurrentReasonForNonReferral NonReferralReasonARVProgram value.facilityNonReferralReasons

                                            FacilityNCDProgram ->
                                                getCurrentReasonForNonReferral NonReferralReasonNCDProgram value.facilityNonReferralReasons

                                            FacilityANCServices ->
                                                getCurrentReasonForNonReferral NonReferralReasonANCServices value.facilityNonReferralReasons

                                            FacilityHealthCenter ->
                                                -- We should never get here.
                                                Nothing
                                    )

                        suffix =
                            Maybe.map
                                (\reason_ ->
                                    if reason_ == NoReasonForNonReferral then
                                        ""

                                    else
                                        " - " ++ (String.toLower <| translate language <| Translate.ReasonForNonReferral reason_)
                                )
                                reason
                                |> Maybe.withDefault ""
                    in
                    diagnosisForProgressReport
                        ++ complications
                        ++ " - "
                        ++ (String.toLower <| translate language <| Translate.ReferredToFacilityNot facility)
                        ++ " "
                        ++ (String.toLower <| translate language Translate.On)
                        ++ " "
                        ++ formatDDMMYYYY date
                        ++ suffix
                        |> wrapWithLI

        undeterminedDiagnosisMessage =
            diagnosisForProgressReport
                ++ " - "
                ++ translate language Translate.UndeterminedDiagnosisMessage
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        noTreatmentRecordedMessage =
            noTreatmentRecordedMessageWithComplications ""

        noTreatmentRecordedMessageWithComplications complication =
            diagnosisForProgressReport
                ++ complication
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ (String.toLower <| translate language Translate.NoTreatmentRecorded)
                |> wrapWithLI

        noTreatmentAdministeredMessage =
            diagnosisForProgressReport
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                ++ " - "
                ++ (String.toLower <| translate language Translate.NoTreatmentAdministered)
                |> wrapWithLI

        treatmentMessageForMedication =
            translate language Translate.TreatedWith
                |> customTreatmentMessageForMedication

        treatmentMessageForMedicationLower =
            translate language Translate.TreatedWith
                |> String.toLower
                |> customTreatmentMessageForMedication

        customTreatmentMessageForMedication treatmentLabel distributionSigns nonAdministrationReasons medication =
            if EverySet.member medication distributionSigns then
                treatmentLabel
                    ++ " "
                    ++ (translate language <| Translate.MedicationDistributionSign medication)
                    |> Just

            else
                Dict.get medication nonAdministrationReasons
                    |> Maybe.map
                        (\nonAdministrationReason ->
                            translate language Translate.TreatedWithNot
                                ++ " "
                                ++ (translate language <| Translate.MedicationDistributionSign medication)
                                ++ " "
                                ++ (String.toLower <| translate language Translate.DueTo)
                                ++ " "
                                ++ (translate language <| Translate.AdministrationNote nonAdministrationReason)
                        )

        diagnosisTreatedWithOnDateMessage recommendedTreatmentSign =
            diagnosisForProgressReport
                ++ " - "
                ++ (String.toLower <| translate language Translate.TreatedWith)
                ++ " "
                ++ (translate language <| Translate.RecommendedTreatmentSignLabel recommendedTreatmentSign)
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI
    in
    case diagnosis of
        DiagnosisHIV ->
            getMeasurementValueFunc measurements.sendToHC
                |> Maybe.map
                    (\value ->
                        let
                            refferedToARVProgram =
                                Maybe.map (EverySet.member ReferToARVProgram)
                                    value.referToFacilitySigns
                                    |> Maybe.withDefault False
                        in
                        if refferedToARVProgram then
                            referredToFacilityMessage FacilityARVProgram

                        else
                            getMeasurementValueFunc measurements.medicationDistribution
                                |> Maybe.andThen
                                    (\value_ ->
                                        let
                                            nonAdministrationReasons =
                                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value_
                                        in
                                        Maybe.map2
                                            (\tdf3TCTreatmentMessage dolutegravirTreatmentmessage ->
                                                let
                                                    diagnosisMessage =
                                                        diagnosisForProgressReport
                                                            ++ " "
                                                            ++ (String.toLower <| translate language Translate.On)
                                                            ++ " "
                                                            ++ formatDDMMYYYY date
                                                in
                                                [ li []
                                                    [ p [] [ text diagnosisMessage ]
                                                    , p [] [ text tdf3TCTreatmentMessage ]
                                                    , p [] [ text dolutegravirTreatmentmessage ]
                                                    ]
                                                ]
                                            )
                                            (treatmentMessageForMedication value_.distributionSigns nonAdministrationReasons TDF3TC)
                                            (treatmentMessageForMedication value_.distributionSigns nonAdministrationReasons Dolutegravir)
                                    )
                                |> Maybe.withDefault noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisHIVDetectableViralLoad ->
            getMeasurementValueFunc measurements.hivPCRTest
                |> Maybe.andThen .hivViralLoad
                |> Maybe.map
                    (\viralLoad ->
                        diagnosisForProgressReportToString language diagnosis
                            ++ " "
                            ++ (String.toLower <| translate language Translate.On)
                            ++ " "
                            ++ formatDDMMYYYY date
                            ++ " -- "
                            ++ String.fromFloat viralLoad
                            |> wrapWithLI
                    )
                |> Maybe.withDefault []

        DiagnosisDiscordantPartnership ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        treatmentMessageForMedication value.distributionSigns nonAdministrationReasons TDF3TC
                            |> Maybe.map
                                (\tdf3TCTreatmentMessage ->
                                    let
                                        diagnosisMessage =
                                            diagnosisForProgressReport
                                                ++ " "
                                                ++ (String.toLower <| translate language Translate.On)
                                                ++ " "
                                                ++ formatDDMMYYYY date
                                    in
                                    [ li []
                                        [ p [] [ text diagnosisMessage ]
                                        , p [] [ text tdf3TCTreatmentMessage ]
                                        ]
                                    ]
                                )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisSyphilis ->
            syphilisTreatmentMessage ""

        DiagnosisSyphilisWithComplications ->
            let
                complications =
                    getMeasurementValueFunc measurements.syphilisTest
                        |> Maybe.andThen .symptoms
                        |> Maybe.map
                            (\symptoms ->
                                if EverySet.isEmpty symptoms then
                                    ""

                                else if EverySet.member NoIllnessSymptoms symptoms then
                                    ""

                                else
                                    " - ["
                                        ++ (EverySet.toList symptoms
                                                |> List.map (Translate.IllnessSymptom >> translate language)
                                                |> String.join ", "
                                           )
                                        ++ "]"
                            )
                        |> Maybe.withDefault ""
            in
            syphilisTreatmentMessage complications

        DiagnosisChronicHypertensionImmediate ->
            hypertensionTreatmentMessage

        DiagnosisChronicHypertensionAfterRecheck ->
            hypertensionTreatmentMessage

        DiagnosisGestationalHypertensionImmediate ->
            hypertensionTreatmentMessage

        DiagnosisGestationalHypertensionAfterRecheck ->
            hypertensionTreatmentMessage

        DiagnosisEclampsia ->
            referredToHospitalMessage

        DiagnosisMiscarriage ->
            referredToHospitalMessage

        DiagnosisMolarPregnancy ->
            referredToHospitalMessage

        DiagnosisPlacentaPrevia ->
            referredToHospitalMessage

        DiagnosisPlacentalAbruption ->
            referredToHospitalMessage

        DiagnosisUterineRupture ->
            referredToHospitalMessage

        DiagnosisObstructedLabor ->
            referredToHospitalMessage

        DiagnosisPostAbortionSepsis ->
            referredToHospitalMessage

        DiagnosisEctopicPregnancy ->
            referredToHospitalMessage

        DiagnosisPROM ->
            referredToHospitalMessage

        DiagnosisPPROM ->
            referredToHospitalMessage

        DiagnosisHyperemesisGravidum ->
            referredToHospitalMessage

        DiagnosisHyperemesisGravidumBySymptoms ->
            referredToHospitalMessage

        DiagnosisSevereVomiting ->
            referredToHospitalMessage

        DiagnosisSevereVomitingBySymptoms ->
            referredToHospitalMessage

        DiagnosisMaternalComplications ->
            referredToHospitalMessage

        DiagnosisInfection ->
            referredToHospitalMessage

        DiagnosisImminentDelivery ->
            referredToHospitalMessage

        DiagnosisLaborAndDelivery ->
            referredToHospitalMessage

        DiagnosisModerateAnemia ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        Maybe.map2
                            (\ironTreatmentMessage folicAcidTreatmentMessage ->
                                let
                                    diagnosisMessage =
                                        diagnosisForProgressReport
                                            ++ " "
                                            ++ (String.toLower <| translate language Translate.On)
                                            ++ " "
                                            ++ formatDDMMYYYY date
                                in
                                [ li []
                                    [ p [] [ text diagnosisMessage ]
                                    , p [] [ text ironTreatmentMessage ]
                                    , p [] [ text folicAcidTreatmentMessage ]
                                    ]
                                ]
                            )
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Iron)
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons FolicAcid)
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisSevereAnemia ->
            referredToHospitalMessage

        DiagnosisSevereAnemiaWithComplications ->
            let
                complication =
                    " - ["
                        ++ (complicationsByExamination
                                ++ complicationsByDangerSigns
                                ++ elevatedRespiratoryRate
                                |> String.join ", "
                           )
                        ++ "]"

                elevatedRespiratoryRate =
                    if respiratoryRateElevated measurements then
                        [ translate language Translate.ElevatedRespiratoryRate ]

                    else
                        []

                complicationsByDangerSigns =
                    getMeasurementValueFunc measurements.dangerSigns
                        |> Maybe.map
                            (\value ->
                                if EverySet.member DifficultyBreathing value.signs then
                                    [ translate language <| Translate.DangerSign DifficultyBreathing ]

                                else
                                    []
                            )
                        |> Maybe.withDefault []

                complicationsByExamination =
                    getMeasurementValueFunc measurements.corePhysicalExam
                        |> Maybe.map
                            (\exam ->
                                let
                                    pallorHands =
                                        if EverySet.member PallorHands exam.hands then
                                            [ translate language Translate.HandPallor ]

                                        else
                                            []

                                    paleConjuctiva =
                                        if EverySet.member PaleConjuctiva exam.eyes then
                                            [ translate language Translate.PaleConjuctiva ]

                                        else
                                            []
                                in
                                pallorHands ++ paleConjuctiva
                            )
                        |> Maybe.withDefault []
            in
            referredToHospitalMessageWithComplications complication

        DiagnosisMalaria ->
            malariaTreatmentMessage

        DiagnosisMalariaMedicatedContinued ->
            referredToHospitalMessage

        DiagnosisMalariaWithAnemia ->
            malariaTreatmentMessage

        DiagnosisMalariaWithAnemiaMedicatedContinued ->
            referredToHospitalMessage

        DiagnosisMalariaWithSevereAnemia ->
            malariaTreatmentMessage ++ referredToHospitalMessage

        DiagnosisHepatitisB ->
            referredToHospitalMessage

        DiagnosisNeurosyphilis ->
            referredToHospitalMessage

        DiagnosisModeratePreeclampsiaInitialPhase ->
            if EverySet.member DiagnosisModeratePreeclampsiaInitialPhase allDiagnoses then
                referredToHospitalMessage

            else
                hypertensionTreatmentMessage

        DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisModeratePreeclampsiaRecurrentPhase ->
            if EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhase allDiagnoses then
                referredToHospitalMessage

            else
                hypertensionTreatmentMessage

        DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaInitialPhase ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaRecurrentPhase ->
            referredToHospitalMessage

        DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
            referredToHospitalMessage

        DiagnosisHeartburn ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentAluminiumHydroxide signs then
                            diagnosisTreatedWithOnDateMessage TreatmentAluminiumHydroxide

                        else if EverySet.member TreatmentHealthEducationForHeartburn signs then
                            noTreatmentAdministeredMessage

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisHeartburnPersistent ->
            referredToHospitalMessage

        DiagnosisDeepVeinThrombosis ->
            let
                location =
                    getMeasurementValueFunc measurements.symptomReview
                        |> Maybe.map
                            (\value ->
                                if EverySet.member SymptomQuestionLegPainRednessLeft value.symptomQuestions then
                                    " (" ++ String.toLower (translate language Translate.LegLeft) ++ ") "

                                else
                                    " (" ++ String.toLower (translate language Translate.LegRight) ++ ") "
                            )
                        |> Maybe.withDefault ""
            in
            referredToHospitalMessageWithComplications location

        DiagnosisPelvicPainIntense ->
            referredToHospitalMessage

        DiagnosisPelvicPainContinued ->
            referredToHospitalMessage

        DiagnosisUrinaryTractInfection ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentNitrofurantoin signs then
                            diagnosisTreatedWithOnDateMessage TreatmentNitrofurantoin

                        else if EverySet.member TreatmentAmoxicillin signs then
                            diagnosisTreatedWithOnDateMessage TreatmentAmoxicillin

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisUrinaryTractInfectionContinued ->
            referredToHospitalMessage

        DiagnosisPyelonephritis ->
            referredToHospitalMessage

        DiagnosisCandidiasis ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (\signs ->
                        if EverySet.member TreatmentClotrimaxazole200 signs then
                            diagnosisTreatedWithOnDateMessage TreatmentClotrimaxazole200

                        else if EverySet.member TreatmentClotrimaxazole500 signs then
                            diagnosisTreatedWithOnDateMessage TreatmentClotrimaxazole500

                        else
                            noTreatmentRecordedMessage
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisCandidiasisContinued ->
            referredToHospitalMessage

        DiagnosisGonorrhea ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        Maybe.map2
                            (\ceftriaxoneMessage azithromycinMessage ->
                                let
                                    diagnosisMessage =
                                        diagnosisForProgressReport
                                            ++ " "
                                            ++ (String.toLower <| translate language Translate.On)
                                            ++ " "
                                            ++ formatDDMMYYYY date
                                in
                                [ li []
                                    [ p [] [ text diagnosisMessage ]
                                    , p [] [ text ceftriaxoneMessage ]
                                    , p [] [ text azithromycinMessage ]
                                    ]
                                ]
                            )
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Ceftriaxone)
                            (treatmentMessageForMedication value.distributionSigns nonAdministrationReasons Azithromycin)
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisGonorrheaContinued ->
            referredToHospitalMessage

        DiagnosisTrichomonasOrBacterialVaginosis ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        treatmentMessageForMedicationLower value.distributionSigns nonAdministrationReasons Metronidazole
                            |> Maybe.map
                                (\message ->
                                    diagnosisForProgressReport
                                        ++ " - "
                                        ++ message
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                                )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisTrichomonasOrBacterialVaginosisContinued ->
            referredToHospitalMessage

        DiagnosisTuberculosis ->
            diagnosisForProgressReport
                ++ " - "
                ++ translate language Translate.TuberculosisInstructionsFollowed
                ++ " "
                ++ (String.toLower <| translate language Translate.On)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI

        DiagnosisDiabetes ->
            referredToHospitalMessage

        DiagnosisGestationalDiabetes ->
            referredToHospitalMessage

        DiagnosisRhesusNegative ->
            referredToHospitalMessage

        DiagnosisDepressionNotLikely ->
            mentalHealthMessage

        DiagnosisDepressionPossible ->
            mentalHealthMessage

        DiagnosisDepressionHighlyPossible ->
            mentalHealthMessage

        DiagnosisDepressionProbable ->
            mentalHealthMessage

        DiagnosisSuicideRisk ->
            mentalHealthMessage

        DiagnosisPostpartumAbdominalPain ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumHeadache ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumFatigue ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumFever ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumPerinealPainOrDischarge ->
            undeterminedDiagnosisMessage

        DiagnosisPostpartumUrinaryIncontinence ->
            referredToHospitalMessage

        DiagnosisPostpartumInfection ->
            referredToHospitalMessage

        DiagnosisPostpartumExcessiveBleeding ->
            referredToHospitalMessage

        DiagnosisPostpartumEarlyMastitisOrEngorgment ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen
                    (\value ->
                        let
                            nonAdministrationReasons =
                                Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                        in
                        treatmentMessageForMedicationLower value.distributionSigns nonAdministrationReasons Paracetamol
                            |> Maybe.map
                                (\message ->
                                    diagnosisForProgressReport
                                        ++ " - "
                                        ++ message
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                                )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisPostpartumMastitis ->
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.andThen .recommendedTreatmentSigns
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter (\sign -> List.member sign recommendedTreatmentSignsForMastitis)
                        >> (\treatment ->
                                if List.isEmpty treatment then
                                    noTreatmentRecordedMessage

                                else if List.member NoTreatmentForMastitis treatment then
                                    noTreatmentAdministeredMessage

                                else
                                    let
                                        treatedWithMessage =
                                            List.head treatment
                                                |> Maybe.map
                                                    (\medication ->
                                                        " - "
                                                            ++ (String.toLower <| translate language Translate.TreatedWith)
                                                            ++ " "
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabel medication)
                                                    )
                                                |> Maybe.withDefault ""
                                    in
                                    diagnosisForProgressReport
                                        ++ treatedWithMessage
                                        ++ " "
                                        ++ (String.toLower <| translate language Translate.On)
                                        ++ " "
                                        ++ formatDDMMYYYY date
                                        |> wrapWithLI
                           )
                    )
                |> Maybe.withDefault noTreatmentRecordedMessage

        DiagnosisOther ->
            -- Other diagnosis is used only at outside care diagnostics.
            []

        NoPrenatalDiagnosis ->
            []


viewTreatmentForOutsideCareDiagnosis :
    Language
    -> NominalDate
    -> Maybe (EverySet OutsideCareMedication)
    -> PrenatalDiagnosis
    -> List (Html any)
viewTreatmentForOutsideCareDiagnosis language date medications diagnosis =
    let
        completePhrase maybeTreatedWithPhrase =
            let
                treatedWith =
                    Maybe.map (\phrase -> ", " ++ phrase)
                        maybeTreatedWithPhrase
                        |> Maybe.withDefault ""
            in
            diagnosisForProgressReportToString language diagnosis
                ++ " - "
                ++ (String.toLower <| translate language <| Translate.DiagnosedByOutsideCare)
                ++ treatedWith
                ++ ", "
                ++ (String.toLower <| translate language Translate.AddedToPatientRecordOn)
                ++ " "
                ++ formatDDMMYYYY date
                |> wrapWithLI
    in
    if List.member diagnosis outsideCareDiagnosesWithPossibleMedication then
        let
            treatedWithPhrase treartmentOptions noTreatmentOption =
                Maybe.map
                    (EverySet.toList
                        >> List.filter (\treatment -> List.member treatment treartmentOptions)
                        >> (\treatments ->
                                if List.isEmpty treatments || List.member noTreatmentOption treatments then
                                    noTreatmentAdministeredPhrase

                                else
                                    " "
                                        ++ (String.toLower <| translate language Translate.TreatedWith)
                                        ++ " "
                                        ++ (List.map
                                                (Translate.OutsideCareMedicationLabel >> translate language)
                                                treatments
                                                |> String.join ", "
                                           )
                                        ++ " "
                           )
                    )
                    medications
                    |> Maybe.withDefault noTreatmentAdministeredPhrase

            noTreatmentAdministeredPhrase =
                " "
                    ++ (String.toLower <| translate language Translate.NoTreatmentAdministered)
                    ++ " "
        in
        case diagnosis of
            DiagnosisHIV ->
                treatedWithPhrase outsideCareMedicationOptionsMalaria NoOutsideCareMedicationForMalaria
                    |> Just
                    |> completePhrase

            DiagnosisSyphilis ->
                treatedWithPhrase outsideCareMedicationOptionsSyphilis NoOutsideCareMedicationForSyphilis
                    |> Just
                    |> completePhrase

            DiagnosisMalaria ->
                treatedWithPhrase outsideCareMedicationOptionsMalaria NoOutsideCareMedicationForMalaria
                    |> Just
                    |> completePhrase

            DiagnosisModerateAnemia ->
                treatedWithPhrase outsideCareMedicationOptionsAnemia NoOutsideCareMedicationForAnemia
                    |> Just
                    |> completePhrase

            DiagnosisGestationalHypertensionImmediate ->
                treatedWithPhrase outsideCareMedicationOptionsHypertension NoOutsideCareMedicationForHypertension
                    |> Just
                    |> completePhrase

            DiagnosisChronicHypertensionImmediate ->
                treatedWithPhrase outsideCareMedicationOptionsHypertension NoOutsideCareMedicationForHypertension
                    |> Just
                    |> completePhrase

            -- Will never get here.
            _ ->
                []

    else if List.member diagnosis outsideCareDiagnoses then
        completePhrase Nothing

    else
        -- Not an outside care diagnosis.
        []
