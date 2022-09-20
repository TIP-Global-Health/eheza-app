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
        , PrenatalHIVSign(..)
        , PrenatalHealthEducationSign(..)
        , PrenatalMeasurements
        , PrenatalOutsideCareMedication(..)
        , PrenatalSymptomQuestion(..)
        , PrenatalTestExecutionNote(..)
        , PrenatalTestResult(..)
        , PrenatalTestVariant(..)
        , ReasonForNonReferral(..)
        , RecommendedTreatmentSign(..)
        , ReferToFacilitySign(..)
        , ReferralFacility(..)
        , SendToHCSign(..)
        , SpecialityCareSign(..)
        , ViralLoadStatus(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, prenatalLabExpirationPeriod)
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
import Measurement.Utils
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask(..))
import Pages.Prenatal.Activity.Utils
    exposing
        ( outsideCareMedicationOptionsAnemia
        , outsideCareMedicationOptionsHIV
        , outsideCareMedicationOptionsHypertension
        , outsideCareMedicationOptionsMalaria
        , outsideCareMedicationOptionsSyphilis
        , respiratoryRateElevated
        )
import Pages.Prenatal.DemographicsReport.View exposing (viewItemHeading)
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
        , getCurrentReasonForNonReferral
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
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PrenatalEncounterPage id)

                InitiatorRecurrentEncounterPage prenatalEncounterId ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id)

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
                    , viewLabsPane language currentDate assembled
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
        allNurseEncountersData =
            assembled.nursePreviousEncountersData
                ++ (if isChw then
                        []

                    else
                        [ { startDate = assembled.encounter.startDate
                          , diagnoses = assembled.encounter.diagnoses
                          , pastDiagnoses = assembled.encounter.pastDiagnoses
                          , measurements = assembled.measurements
                          }
                        ]
                   )
                |> List.sortWith (sortByDateDesc .startDate)

        dignoses =
            List.map
                (\data ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses data.startDate data.diagnoses assembled medicalDiagnoses

                        diagnosesEntries =
                            List.map (viewTreatmentForDiagnosis language data.startDate data.measurements data.diagnoses) diagnosesIncludingChronic
                                |> List.concat

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis medicalDiagnoses)
                                                >> List.map (viewTreatmentForOutsideCareDiagnosis language data.startDate value.medications)
                                                >> List.concat
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        knownAsPositiveEntries =
                            viewKnownPositives language data.startDate data.measurements

                        programReferralEntries =
                            getMeasurementValueFunc data.measurements.specialityCare
                                |> Maybe.map
                                    (\value ->
                                        let
                                            arvEntry =
                                                resolveARVReferralDiagnosis assembled.nursePreviousEncountersData
                                                    |> Maybe.map
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language data.startDate diagnosis FacilityARVProgram

                                                            else
                                                                []
                                                        )
                                                    |> Maybe.withDefault []

                                            ncdEntries =
                                                resolveNCDReferralDiagnoses assembled.nursePreviousEncountersData
                                                    |> List.map
                                                        (\diagnosis ->
                                                            if not <| EverySet.member EnrolledToARVProgram value then
                                                                viewProgramReferralEntry language data.startDate diagnosis FacilityNCDProgram

                                                            else
                                                                []
                                                        )
                                                    |> List.concat
                                        in
                                        arvEntry ++ ncdEntries
                                    )
                                |> Maybe.withDefault []

                        pastDiagnosesEntries =
                            EverySet.toList data.pastDiagnoses
                                |> List.filter (\diagnosis -> List.member diagnosis medicalDiagnoses)
                                |> List.map (viewTreatmentForPastDiagnosis language data.startDate)
                                |> List.concat
                    in
                    knownAsPositiveEntries
                        ++ diagnosesEntries
                        ++ outsideCareDiagnosesEntries
                        ++ pastDiagnosesEntries
                        ++ programReferralEntries
                )
                allNurseEncountersData
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
        allNurseEncountersData =
            assembled.nursePreviousEncountersData
                ++ (if isChw then
                        []

                    else
                        [ { startDate = assembled.encounter.startDate
                          , diagnoses = assembled.encounter.diagnoses
                          , pastDiagnoses = assembled.encounter.pastDiagnoses
                          , measurements = assembled.measurements
                          }
                        ]
                   )
                |> List.sortWith (sortByDateDesc .startDate)

        initialHealthEducationOccurances =
            List.foldr
                (\data accum ->
                    getMeasurementValueFunc data.measurements.healthEducation
                        |> Maybe.map
                            (\value ->
                                let
                                    signRecord sign =
                                        if
                                            EverySet.member sign value.signs
                                                && (isNothing <| Dict.get sign accum)
                                        then
                                            Just ( sign, data.startDate )

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
                allNurseEncountersData

        dignoses =
            List.map
                (\data ->
                    let
                        diagnosesIncludingChronic =
                            updateChronicHypertensionDiagnoses data.startDate data.diagnoses assembled obstetricalDiagnoses

                        diagnosesEntries =
                            List.map (viewTreatmentForDiagnosis language data.startDate data.measurements data.diagnoses) diagnosesIncludingChronic
                                |> List.concat

                        outsideCareDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.outsideCare
                                |> Maybe.andThen
                                    (\value ->
                                        Maybe.map
                                            (EverySet.toList
                                                >> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                                                >> List.map (viewTreatmentForOutsideCareDiagnosis language data.startDate value.medications)
                                                >> List.concat
                                            )
                                            value.diagnoses
                                    )
                                |> Maybe.withDefault []

                        pastDiagnosesEntries =
                            EverySet.toList data.pastDiagnoses
                                |> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                                |> List.map (viewTreatmentForPastDiagnosis language data.startDate)
                                |> List.concat

                        healthEducationDiagnosesEntries =
                            getMeasurementValueFunc data.measurements.healthEducation
                                |> Maybe.map
                                    (\value ->
                                        let
                                            formatedDate =
                                                formatDDMMYYYY data.startDate

                                            messageForSign sign =
                                                if EverySet.member sign value.signs then
                                                    Dict.get sign initialHealthEducationOccurances
                                                        |> Maybe.map
                                                            (\initialDate ->
                                                                let
                                                                    currentIsInitial =
                                                                        Date.compare initialDate data.startDate == EQ
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
                    diagnosesEntries ++ outsideCareDiagnosesEntries ++ pastDiagnosesEntries ++ healthEducationDiagnosesEntries
                )
                allNurseEncountersData
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

        content =
            if not <| List.isEmpty actions then
                div [ class "heading" ]
                    [ div [ class "date" ] [ text <| translate language Translate.Date ]
                    , div [ class "chw-actions" ] [ text <| translate language Translate.Actions ]
                    ]
                    :: actions

            else
                []

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
        , div [ class "pane-content" ]
            content
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
        allNurseEncountersData =
            List.map (\data -> ( data.startDate, data.measurements )) assembled.nursePreviousEncountersData
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.measurements ) ]
                   )

        allMeasurements =
            List.map Tuple.second allNurseEncountersData

        encountersTrimestersData =
            allNurseEncountersData
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
            allNurseEncountersData
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalMovement >> (==) True)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        fetalHeartRateDate =
            allNurseEncountersData
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
            allNurseEncountersData
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
            allNurseEncountersData
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
                    , heightWeightBMITable language currentDate assembled.globalLmpDate allNurseEncountersData
                    , viewBMIForEGA language egaBmiValues
                    , illustrativePurposes language
                    ]
                , div [ class "fundal-height-info" ]
                    [ viewChartHeading Translate.FundalHeight
                    , fundalHeightTable language currentDate assembled.globalLmpDate allNurseEncountersData
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


viewLabsPane : Language -> NominalDate -> AssembledData -> Html Msg
viewLabsPane language currentDate assembled =
    div [ class "labs" ] <|
        [ viewItemHeading language Translate.LabResults "blue"
        , div [ class "pane-content" ]
            [ div
                [ class "ui primary button"
                , onClick <| SetLabResultsMode <| Just (LabResultsCurrent LabResultsCurrentMain)
                ]
                [ text <| translate language Translate.SeeLabResults ]
            ]
        ]


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
                :: List.map .measurements assembled.nursePreviousEncountersData

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
                    [ viewLabResultsEntry language currentDate (LabResultsHistoryHIV hivTestResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryHIVPCR hivPCRTestResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistorySyphilis syphilisTestResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryHepatitisB hepatitisBTestResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryMalaria malariaTestResults)
                    , dipstickShortEntry
                    , dipstickLongEntry
                    , viewLabResultsEntry language currentDate (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryHemoglobin hemoglobinResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryBloodGroup bloodGroupResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryRhesus rhesusResults)
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
                    , viewLabResultsEntry language currentDate (LabResultsHistoryLeukocytes leukocytesResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryNitrite nitriteResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryUrobilinogen urobilinogenResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryHaemoglobin haemoglobinResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryKetone ketoneResults)
                    , viewLabResultsEntry language currentDate (LabResultsHistoryBilirubin bilirubinResults)
                    ]

        proteinEntry =
            viewLabResultsEntry language currentDate (LabResultsHistoryProtein proteinResults)

        phEntry =
            viewLabResultsEntry language currentDate (LabResultsHistoryPH phResults)

        glucoseEntry =
            viewLabResultsEntry language currentDate (LabResultsHistoryGlucose glucoseResults)

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


viewLabResultsEntry : Language -> NominalDate -> LabResultsHistoryMode -> Html Msg
viewLabResultsEntry language currentDate results =
    let
        config =
            case results of
                LabResultsHistoryHIV assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHIVTest
                    , recentResult = Maybe.map (translatePrenatalTestReport language) recentResultValue
                    , knownAsPositive = recentResultValue == Just TestNotPerformedKnownAsPositive
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hivResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHIVPCR assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHIVPCRTest
                    , recentResult = Maybe.map (Translate.HIVPCRResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hivPCRResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistorySyphilis assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskSyphilisTest
                    , recentResult = Maybe.map (Translate.PrenatalTestResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map syphilisResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHepatitisB assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHepatitisBTest
                    , recentResult = Maybe.map (translatePrenatalTestReport language) recentResultValue
                    , knownAsPositive = recentResultValue == Just TestNotPerformedKnownAsPositive
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hepatitisBResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryMalaria assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskMalariaTest
                    , recentResult = Maybe.map (Translate.PrenatalTestResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map malariaResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryProtein assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryProteinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryProteinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map proteinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryPH assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryPHLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryPHValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map phResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryGlucose assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryGlucoseLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryGlucoseValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map glucoseResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryLeukocytes assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryLeukocytesLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryLeukocytesValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map leukocytesResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryNitrite assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryNitriteLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryNitriteValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map nitriteResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryUrobilinogen assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryUrobilinogenLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map urobilinogenResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHaemoglobin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryHaemoglobinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map urineHaemoglobinValueResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryKetone assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryKetoneLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryKetoneValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map ketoneResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBilirubin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryBilirubinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryBilirubinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map bilirubinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryRandomBloodSugar assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskRandomBloodSugarTest
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map randomBloodSugarResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHemoglobin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHemoglobinTest
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hemoglobinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBloodGroup assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryBloodGroupLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryBloodGroup >> translate language) recentResultValue
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , knownAsPositive = False
                    , totalResults = List.length assembled
                    , recentResultNormal = True
                    }

                LabResultsHistoryRhesus assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryRhesusLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryRhesus >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map rhesusResultsNormal recentResultValue
                            |> Maybe.withDefault True
                    }

        dateCell =
            Maybe.map (formatDDMMYYYY >> text) config.recentResultDate
                |> Maybe.withDefault (text "")

        resultCell =
            if config.totalResults == 0 then
                text ""

            else if config.knownAsPositive then
                viewKnownAsPositiveResult language

            else
                Maybe.map text config.recentResult
                    |> Maybe.withDefault (viewUncompetedResult language currentDate config.recentResultDate)

        historyResultsIcon =
            if config.totalResults > 1 then
                div
                    [ class "icon-forward"
                    , onClick <| SetLabResultsMode <| Just (LabResultsHistory results)
                    ]
                    []

            else
                emptyNode
    in
    div [ classList [ ( "entry", True ), ( "warning", not config.recentResultNormal ) ] ]
        [ div [ class "name" ] [ translateText language config.label ]
        , div [ class "date" ] [ dateCell ]
        , div [ class "result" ] [ resultCell ]
        , div [ class "normal-range" ] [ text <| translate language <| Translate.LabResultsNormalRange results ]
        , historyResultsIcon
        ]


translatePrenatalTestReport : Language -> PrenatalTestReport -> String
translatePrenatalTestReport language report =
    case report of
        TestPerformed result ->
            Translate.PrenatalTestResult result
                |> translate language

        TestNotPerformedKnownAsPositive ->
            -- We don't need this, since this result is displayed
            -- using viewKnownAsPositiveResult funciton.
            ""


viewKnownAsPositiveResult : Language -> Html any
viewKnownAsPositiveResult language =
    span [ class "known-positive" ]
        [ text <| translate language Translate.KnownPositive ]


viewUncompetedResult : Language -> NominalDate -> Maybe NominalDate -> Html any
viewUncompetedResult language currentDate resultDate =
    let
        transId =
            Maybe.map
                (\date ->
                    if Date.diff Days date currentDate >= prenatalLabExpirationPeriod then
                        Translate.ResultsMissing

                    else
                        Translate.ResultsPending
                )
                resultDate
                |> Maybe.withDefault Translate.ResultsPending
    in
    span [ class "uncompleted" ] [ translateText language transId ]


viewLabResultsHistoryPane : Language -> NominalDate -> LabResultsHistoryMode -> Html Msg
viewLabResultsHistoryPane language currentDate mode =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                , div [ class "normal-range" ] [ translateText language Translate.NormalRange ]
                ]

        entries =
            case mode of
                LabResultsHistoryHIV assembled ->
                    List.map (viewEntry (translatePrenatalTestReport language) hivResultNormal) assembled

                LabResultsHistoryHIVPCR assembled ->
                    List.map (viewEntry (Translate.HIVPCRResult >> translate language) hivPCRResultNormal) assembled

                LabResultsHistorySyphilis assembled ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language) syphilisResultNormal) assembled

                LabResultsHistoryHepatitisB assembled ->
                    List.map (viewEntry (translatePrenatalTestReport language) hepatitisBResultNormal) assembled

                LabResultsHistoryMalaria assembled ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language) malariaResultNormal) assembled

                LabResultsHistoryProtein assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryProteinValue >> translate language) proteinResultNormal) assembled

                LabResultsHistoryPH assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryPHValue >> translate language) phResultNormal) assembled

                LabResultsHistoryGlucose assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryGlucoseValue >> translate language) glucoseResultNormal) assembled

                LabResultsHistoryLeukocytes assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryLeukocytesValue >> translate language) leukocytesResultNormal) assembled

                LabResultsHistoryNitrite assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryNitriteValue >> translate language) nitriteResultNormal) assembled

                LabResultsHistoryUrobilinogen assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language) urobilinogenResultNormal) assembled

                LabResultsHistoryHaemoglobin assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language) urineHaemoglobinValueResultNormal) assembled

                LabResultsHistoryKetone assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryKetoneValue >> translate language) ketoneResultNormal) assembled

                LabResultsHistoryBilirubin assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBilirubinValue >> translate language) bilirubinResultNormal) assembled

                LabResultsHistoryRandomBloodSugar assembled ->
                    List.map (viewEntry String.fromFloat randomBloodSugarResultNormal) assembled

                LabResultsHistoryHemoglobin assembled ->
                    List.map (viewEntry String.fromFloat hemoglobinResultNormal) assembled

                LabResultsHistoryBloodGroup assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBloodGroup >> translate language) (always True)) assembled

                LabResultsHistoryRhesus assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryRhesus >> translate language) rhesusResultsNormal) assembled

        viewEntry resultToStringFunc resultNormalFunc ( date, maybeResult ) =
            let
                resultCell =
                    Maybe.map (resultToStringFunc >> text) maybeResult
                        |> Maybe.withDefault (viewUncompetedResult language currentDate (Just date))

                resultNormal =
                    Maybe.map resultNormalFunc maybeResult
                        |> Maybe.withDefault True

                warningIcon =
                    if not resultNormal then
                        img [ class "icon-warning", src "assets/images/exclamation-red.png" ] []

                    else
                        emptyNode
            in
            div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
                [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                , div [ class "result" ] [ resultCell ]
                , div [ class "normal-range" ] [ text <| translate language <| Translate.LabResultsNormalRange mode ]
                , warningIcon
                ]
    in
    div [ class "lab-results-history" ]
        [ viewItemHeading language (Translate.LabResultsHistoryModeLabel mode) "blue"
        , div [ class "pane-content" ] [ heading ]
        , div [ class "group-content" ] entries
        ]


viewProgressPhotosPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewProgressPhotosPane language currentDate isChw assembled =
    let
        allNurseEncountersData =
            List.map (\data -> ( data.startDate, data.measurements )) assembled.nursePreviousEncountersData
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, assembled.measurements ) ]
                   )

        content =
            allNurseEncountersData
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
                                                            ++ (translate language <| Translate.RecommendedTreatmentSignLabelForProgressReport medication)
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
            malariaTreatmentMessage

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
    -> Maybe (EverySet PrenatalOutsideCareMedication)
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
            treatmentForHypertensionMessage =
                treatedWithPhrase outsideCareMedicationOptionsHypertension NoOutsideCareMedicationForHypertension
                    |> Just
                    |> completePhrase

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
                                                (Translate.PrenatalOutsideCareMedicationLabel >> translate language)
                                                treatments
                                                |> String.join ", "
                                           )
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
                treatedWithPhrase outsideCareMedicationOptionsHIV NoOutsideCareMedicationForMalaria
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
                treatmentForHypertensionMessage

            DiagnosisChronicHypertensionImmediate ->
                treatmentForHypertensionMessage

            DiagnosisModeratePreeclampsiaInitialPhase ->
                treatmentForHypertensionMessage

            -- Will never get here.
            _ ->
                []

    else if List.member diagnosis outsideCareDiagnoses then
        completePhrase Nothing

    else
        -- Not an outside care diagnosis.
        []


viewTreatmentForPastDiagnosis : Language -> NominalDate -> PrenatalDiagnosis -> List (Html any)
viewTreatmentForPastDiagnosis language date diagnosis =
    diagnosisForProgressReportToString language diagnosis
        ++ " - "
        ++ (String.toLower <| translate language Translate.DiagnosedOn)
        ++ " "
        ++ formatDDMMYYYY date
        ++ " "
        ++ (String.toLower <| translate language Translate.PastDiagnosisReportReason)
        ++ "."
        |> wrapWithLI
