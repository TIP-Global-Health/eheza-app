module Pages.AcuteIllnessEncounter.Utils exposing (activityCompleted, covid19Diagnosed, expectActivity, exposureTasksCompleted, generateAssembledData, generatePreviousMeasurements, mandatoryActivitiesCompleted, resolveAcuteIllnessDiagnosis, resolveExposureTasks, resolveLaboratoryTasks)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AcuteFindingsGeneralSign(..)
        , AcuteFindingsRespiratorySign(..)
        , AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecomendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaRapidTestResult(..)
        , ReasonForNotIsolating(..)
        , ResponsePeriod(..)
        , SymptomsGIDerivedSign(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllnessActivity.Model exposing (ExposureTask(..), LaboratoryTask(..))
import Pages.AcuteIllnessEncounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : AcuteIllnessEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> RemoteData.withDefault []

        previousMeasurements =
            List.map Tuple.second previousMeasurementsWithDates
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generatePreviousMeasurements : AcuteIllnessEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, AcuteIllnessMeasurements ))
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.acuteIllnessEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (Dict.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case Dict.get encounterId db.acuteIllnessMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, data )

                                _ ->
                                    Nothing
                    )
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date1 date2)
            )


resolveExposureTasks : AcuteIllnessMeasurements -> Bool -> List ExposureTask
resolveExposureTasks measurements isSuspected =
    let
        expectTask task =
            if isSuspected then
                case task of
                    ExposureTravel ->
                        isJust measurements.travelHistory

                    ExposureExposure ->
                        isJust measurements.exposure

                    ExposureIsolation ->
                        True

                    ExposureContactHC ->
                        True

            else
                case task of
                    ExposureTravel ->
                        True

                    ExposureExposure ->
                        True

                    ExposureIsolation ->
                        False

                    ExposureContactHC ->
                        False
    in
    [ ExposureTravel, ExposureExposure, ExposureIsolation, ExposureContactHC ]
        |> List.filter expectTask


resolveLaboratoryTasks : AcuteIllnessMeasurements -> List LaboratoryTask
resolveLaboratoryTasks measurements =
    let
        diagnosis =
            resolveAcuteIllnessDiagnosis measurements

        expectTask task =
            case task of
                LaboratoryMalariaTesting ->
                    True

                LaboratoryMedicationDistribution ->
                    diagnosis == Just DiagnosisMalariaUncomplicated

                LaboratorySendToHC ->
                    diagnosis == Just DiagnosisMalariaComplicated
    in
    [ LaboratoryMalariaTesting, LaboratoryMedicationDistribution, LaboratorySendToHC ]
        |> List.filter expectTask


exposureTasksCompleted : AcuteIllnessMeasurements -> Bool -> Bool
exposureTasksCompleted measurements isSuspected =
    resolveExposureTasks measurements isSuspected
        |> List.all
            (\task ->
                case task of
                    ExposureTravel ->
                        isJust measurements.travelHistory

                    ExposureExposure ->
                        isJust measurements.exposure

                    ExposureIsolation ->
                        isJust measurements.isolation

                    ExposureContactHC ->
                        isJust measurements.hcContact
            )


expectActivity : NominalDate -> AcuteIllnessMeasurements -> Bool -> AcuteIllnessActivity -> Bool
expectActivity currentDate measurements covid19Suspected activity =
    case activity of
        AcuteIllnessLaboratory ->
            not covid19Suspected
                && mandatoryActivitiesCompleted measurements
                && feverRecorded measurements

        _ ->
            True


activityCompleted : AcuteIllnessMeasurements -> Bool -> AcuteIllnessActivity -> Bool
activityCompleted measurements isSuspected activity =
    case activity of
        AcuteIllnessSymptoms ->
            isJust measurements.symptomsGeneral
                && isJust measurements.symptomsRespiratory
                && isJust measurements.symptomsGI

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals

        AcuteIllnessPriorTreatment ->
            isJust measurements.treatmentReview

        AcuteIllnessLaboratory ->
            isJust measurements.malariaTesting

        AcuteIllnessExposure ->
            exposureTasksCompleted measurements isSuspected


{-| These are the activities that are mandatory, for us to come up with diagnosis.
Covid19 diagnosis is special, therefore, we assume here that Covid19 is negative.
-}
mandatoryActivitiesCompleted : AcuteIllnessMeasurements -> Bool
mandatoryActivitiesCompleted measurements =
    [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPhysicalExam ]
        |> List.all (activityCompleted measurements False)


resolveAcuteIllnessDiagnosis : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosis measurements =
    -- First we check for Covid19.
    if covid19Diagnosed measurements then
        Just DiagnosisCovid19
        -- Check that we have enough data to make a decision on diagnosis.

    else if not (mandatoryActivitiesCompleted measurements) then
        Nothing

    else
        resolveAcuteIllnessDiagnosisByLaboratoryResults measurements


resolveAcuteIllnessDiagnosisByLaboratoryResults : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosisByLaboratoryResults measurements =
    malariaRapidTestResult measurements
        |> Maybe.andThen
            (\testResult ->
                case testResult of
                    RapidTestNegative ->
                        Nothing

                    RapidTestPositive ->
                        if malarialDangerSignsPresent measurements then
                            Just DiagnosisMalariaComplicated

                        else
                            Just DiagnosisMalariaUncomplicated

                    RapidTestIndeterminate ->
                        Nothing
            )


covid19Diagnosed : AcuteIllnessMeasurements -> Bool
covid19Diagnosed measurements =
    let
        contactedPeopleWithSymptoms =
            measurements.exposure
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> EverySet.member COVID19Symptoms
                    )
                |> Maybe.withDefault False

        calculateSymptoms measurement_ getSetFunc defaults =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> getSetFunc

                            setSize =
                                Dict.size set
                        in
                        case setSize of
                            1 ->
                                if List.any (\default -> Dict.member default set) defaults then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        calculateSigns measurement_ default =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                EverySet.size set
                        in
                        case setSize of
                            1 ->
                                if EverySet.member default set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        totalSymptoms =
            calculateSymptoms measurements.symptomsGeneral .value [ NoSymptomsGeneral, SymptomGeneralFever ]
                + calculateSymptoms measurements.symptomsRespiratory .value [ NoSymptomsRespiratory ]
                + calculateSymptoms measurements.symptomsGI (.value >> .signs) [ NoSymptomsGI ]

        totalSigns =
            calculateSigns measurements.travelHistory NoTravelHistorySigns
                + calculateSigns measurements.exposure NoExposureSigns

        feverSymptoms =
            feverAtSymptoms measurements

        feverToday =
            feverAtPhysicalExam measurements

        rdtResult =
            malariaRapidTestResult measurements

        gotFever =
            feverSymptoms
                || (feverToday && rdtResult == Just RapidTestNegative)
    in
    contactedPeopleWithSymptoms || (totalSigns > 0 && (gotFever || totalSymptoms > 1))


feverRecorded : AcuteIllnessMeasurements -> Bool
feverRecorded measurements =
    feverAtSymptoms measurements || feverAtPhysicalExam measurements


feverAtSymptoms : AcuteIllnessMeasurements -> Bool
feverAtSymptoms measurements =
    measurements.symptomsGeneral
        |> Maybe.map
            (\measurement ->
                let
                    feverPeriod =
                        Tuple.second measurement
                            |> .value
                            |> Dict.get SymptomGeneralFever
                            |> Maybe.withDefault 0
                in
                feverPeriod > 0
            )
        |> Maybe.withDefault False


feverAtPhysicalExam : AcuteIllnessMeasurements -> Bool
feverAtPhysicalExam measurements =
    measurements.vitals
        |> Maybe.map
            (\measurement ->
                let
                    bodyTemperature =
                        Tuple.second measurement
                            |> .value
                            |> .bodyTemperature
                in
                bodyTemperature >= 37.5
            )
        |> Maybe.withDefault False


malariaRapidTestResult : AcuteIllnessMeasurements -> Maybe MalariaRapidTestResult
malariaRapidTestResult measurements =
    measurements.malariaTesting
        |> Maybe.map (Tuple.second >> .value)


malarialDangerSignsPresent : AcuteIllnessMeasurements -> Bool
malarialDangerSignsPresent measurements =
    Maybe.map3
        (\symptomsGeneral symptomsGI acuteFindings ->
            let
                symptomsGeneralDict =
                    Tuple.second symptomsGeneral |> .value

                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs

                symptomsGISet =
                    Tuple.second symptomsGI |> .value |> .derivedSigns

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value

                symptomAppearsAtDict symptom dict =
                    Dict.get symptom dict
                        |> Maybe.map ((<) 0)
                        |> Maybe.withDefault False

                lethargy =
                    symptomAppearsAtDict Lethargy symptomsGeneralDict
                        || EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                poorSuck =
                    symptomAppearsAtDict PoorSuck symptomsGeneralDict
                        || EverySet.member AcuteFindingsPoorSuck acuteFindingsValue.signsGeneral

                unableToDrink =
                    symptomAppearsAtDict UnableToDrink symptomsGeneralDict

                unableToEat =
                    symptomAppearsAtDict UnableToEat symptomsGeneralDict

                severeWeakness =
                    symptomAppearsAtDict SevereWeakness symptomsGeneralDict

                cokeColoredUrine =
                    symptomAppearsAtDict CokeColoredUrine symptomsGeneralDict

                convulsions =
                    symptomAppearsAtDict SymptomsGeneralConvulsions symptomsGeneralDict

                spontaneousBleeding =
                    symptomAppearsAtDict SpontaneousBleeding symptomsGeneralDict

                unconsciousness =
                    EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                jaundice =
                    EverySet.member Jaundice acuteFindingsValue.signsGeneral

                stridor =
                    EverySet.member Stridor acuteFindingsValue.signsRespiratory

                nasalFlaring =
                    EverySet.member NasalFlaring acuteFindingsValue.signsRespiratory

                severeWheezing =
                    EverySet.member SevereWheezing acuteFindingsValue.signsRespiratory

                subCostalRetractions =
                    EverySet.member SubCostalRetractions acuteFindingsValue.signsRespiratory

                vomiting =
                    symptomAppearsAtDict Vomiting symptomsGIDict

                intractableVomiting =
                    EverySet.member IntractableVomiting symptomsGISet

                increasedThirst =
                    symptomAppearsAtDict IncreasedThirst symptomsGeneralDict

                dryMouth =
                    symptomAppearsAtDict DryMouth symptomsGeneralDict

                severeDehydration =
                    intractableVomiting && increasedThirst && dryMouth
            in
            lethargy
                || poorSuck
                || unableToDrink
                || unableToEat
                || severeWeakness
                || cokeColoredUrine
                || convulsions
                || spontaneousBleeding
                || unconsciousness
                || jaundice
                || stridor
                || nasalFlaring
                || severeWheezing
                || subCostalRetractions
                || vomiting
                || severeDehydration
        )
        measurements.symptomsGeneral
        measurements.symptomsGI
        measurements.acuteFindings
        |> Maybe.withDefault False
