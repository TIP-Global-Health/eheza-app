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
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
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


resolveLaboratoryTasks : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> List LaboratoryTask
resolveLaboratoryTasks currentDate person diagnosis =
    let
        ( ageMonth0To2, ageMonth2To60 ) =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ( ageMonths < 2, ageMonths >= 2 && ageMonths < 60 ))
                |> Maybe.withDefault ( False, False )

        expectTask task =
            case task of
                LaboratoryMalariaTesting ->
                    True

                LaboratoryMedicationDistribution ->
                    (diagnosis == Just DiagnosisMalariaUncomplicated)
                        || (diagnosis == Just DiagnosisGastrointestinalInfectionUncomplicated)
                        || (diagnosis == Just DiagnosisSimpleColdAndCough)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonth2To60)

                LaboratorySendToHC ->
                    (diagnosis == Just DiagnosisMalariaComplicated)
                        || (diagnosis == Just DiagnosisGastrointestinalInfectionComplicated)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonth0To2)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionComplicated)
                        || (diagnosis == Just DiagnosisFeverOfUnknownOrigin)
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


resolveAcuteIllnessDiagnosis : NominalDate -> Person -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosis currentDate person measurements =
    -- First we check for Covid19.
    if covid19Diagnosed measurements then
        Just DiagnosisCovid19

    else
        resolveNonCovid19AcuteIllnessDiagnosis currentDate person measurements


resolveNonCovid19AcuteIllnessDiagnosis : NominalDate -> Person -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveNonCovid19AcuteIllnessDiagnosis currentDate person measurements =
    -- Verify that we have enough data to make a decision on diagnosis.
    if mandatoryActivitiesCompleted measurements then
        if feverRecorded measurements then
            resolveAcuteIllnessDiagnosisByLaboratoryResults measurements

        else if poorSuckAtSymptoms measurements && respiratoryInfectionDangerSignsPresent measurements then
            Just DiagnosisRespiratoryInfectionComplicated

        else if nonBloodyDiarrheaAtSymptoms measurements then
            Just DiagnosisGastrointestinalInfectionUncomplicated

        else if coughAndNasalCongestionAtSymptoms measurements then
            if respiratoryRateElevatedForChild currentDate person measurements then
                Just DiagnosisRespiratoryInfectionUncomplicated

            else
                Just DiagnosisSimpleColdAndCough

        else
            Nothing

    else
        Nothing


resolveAcuteIllnessDiagnosisByLaboratoryResults : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosisByLaboratoryResults measurements =
    malariaRapidTestResult measurements
        |> Maybe.andThen
            (\testResult ->
                case testResult of
                    RapidTestNegative ->
                        if bloodyDiarrheaAtSymptoms measurements && intractableVomitingAtSymptoms measurements then
                            Just DiagnosisGastrointestinalInfectionComplicated

                        else if respiratoryInfectionDangerSignsPresent measurements then
                            Just DiagnosisRespiratoryInfectionComplicated

                        else
                            Just DiagnosisFeverOfUnknownOrigin

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
        |> Maybe.map (Tuple.second >> .value >> symptomAppearsAtSymptomsDict SymptomGeneralFever)
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


respiratoryRateElevatedForChild : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
respiratoryRateElevatedForChild currentDate person measurements =
    Maybe.map2
        (\measurement ageMonths ->
            let
                respiratoryRate =
                    Tuple.second measurement
                        |> .value
                        |> .respiratoryRate
            in
            if ageMonths < 12 then
                respiratoryRate >= 50

            else if ageMonths < 60 then
                respiratoryRate >= 40

            else
                False
        )
        measurements.vitals
        (ageInMonths currentDate person)
        |> Maybe.withDefault False


bloodyDiarrheaAtSymptoms : AcuteIllnessMeasurements -> Bool
bloodyDiarrheaAtSymptoms measurements =
    measurements.symptomsGI
        |> Maybe.map (Tuple.second >> .value >> .signs >> symptomAppearsAtSymptomsDict BloodyDiarrhea)
        |> Maybe.withDefault False


nonBloodyDiarrheaAtSymptoms : AcuteIllnessMeasurements -> Bool
nonBloodyDiarrheaAtSymptoms measurements =
    measurements.symptomsGI
        |> Maybe.map (Tuple.second >> .value >> .signs >> symptomAppearsAtSymptomsDict NonBloodyDiarrhea)
        |> Maybe.withDefault False


intractableVomitingAtSymptoms : AcuteIllnessMeasurements -> Bool
intractableVomitingAtSymptoms measurements =
    measurements.symptomsGI
        |> Maybe.map (Tuple.second >> .value >> .derivedSigns >> EverySet.member IntractableVomiting)
        |> Maybe.withDefault False


coughAndNasalCongestionAtSymptoms : AcuteIllnessMeasurements -> Bool
coughAndNasalCongestionAtSymptoms measurements =
    measurements.symptomsRespiratory
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (\symptomsDict ->
                        symptomAppearsAtSymptomsDict Cough symptomsDict && symptomAppearsAtSymptomsDict NasalCongestion symptomsDict
                   )
            )
        |> Maybe.withDefault False


poorSuckAtSymptoms : AcuteIllnessMeasurements -> Bool
poorSuckAtSymptoms measurements =
    Maybe.map2
        (\symptomsGeneral acuteFindings ->
            let
                symptomsGeneralDict =
                    Tuple.second symptomsGeneral |> .value

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value
            in
            symptomAppearsAtSymptomsDict PoorSuck symptomsGeneralDict
                || EverySet.member AcuteFindingsPoorSuck acuteFindingsValue.signsGeneral
        )
        measurements.symptomsGeneral
        measurements.acuteFindings
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

                lethargy =
                    symptomAppearsAtSymptomsDict Lethargy symptomsGeneralDict
                        || EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                poorSuck =
                    symptomAppearsAtSymptomsDict PoorSuck symptomsGeneralDict
                        || EverySet.member AcuteFindingsPoorSuck acuteFindingsValue.signsGeneral

                unableToDrink =
                    symptomAppearsAtSymptomsDict UnableToDrink symptomsGeneralDict

                unableToEat =
                    symptomAppearsAtSymptomsDict UnableToEat symptomsGeneralDict

                severeWeakness =
                    symptomAppearsAtSymptomsDict SevereWeakness symptomsGeneralDict

                cokeColoredUrine =
                    symptomAppearsAtSymptomsDict CokeColoredUrine symptomsGeneralDict

                convulsions =
                    symptomAppearsAtSymptomsDict SymptomsGeneralConvulsions symptomsGeneralDict

                spontaneousBleeding =
                    symptomAppearsAtSymptomsDict SpontaneousBleeding symptomsGeneralDict

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
                    symptomAppearsAtSymptomsDict Vomiting symptomsGIDict

                intractableVomiting =
                    EverySet.member IntractableVomiting symptomsGISet

                increasedThirst =
                    symptomAppearsAtSymptomsDict IncreasedThirst symptomsGeneralDict

                dryMouth =
                    symptomAppearsAtSymptomsDict DryMouth symptomsGeneralDict

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


respiratoryInfectionDangerSignsPresent : AcuteIllnessMeasurements -> Bool
respiratoryInfectionDangerSignsPresent measurements =
    Maybe.map3
        (\symptomsGeneral symptomsRespiratory acuteFindings ->
            let
                symptomsGeneralDict =
                    Tuple.second symptomsGeneral |> .value

                symptomsRespiratoryDict =
                    Tuple.second symptomsRespiratory |> .value

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value

                lethargy =
                    symptomAppearsAtSymptomsDict Lethargy symptomsGeneralDict
                        || EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                stridor =
                    EverySet.member Stridor acuteFindingsValue.signsRespiratory

                nasalFlaring =
                    EverySet.member NasalFlaring acuteFindingsValue.signsRespiratory

                severeWheezing =
                    EverySet.member SevereWheezing acuteFindingsValue.signsRespiratory

                subCostalRetractions =
                    EverySet.member SubCostalRetractions acuteFindingsValue.signsRespiratory

                stabbingChestPain =
                    symptomAppearsAtSymptomsDict StabbingChestPain symptomsRespiratoryDict

                shortnessOfBreath =
                    symptomAppearsAtSymptomsDict ShortnessOfBreath symptomsRespiratoryDict
            in
            lethargy
                || stridor
                || nasalFlaring
                || severeWheezing
                || subCostalRetractions
                || stabbingChestPain
                || shortnessOfBreath
        )
        measurements.symptomsGeneral
        measurements.symptomsRespiratory
        measurements.acuteFindings
        |> Maybe.withDefault False


symptomAppearsAtSymptomsDict : a -> Dict a Int -> Bool
symptomAppearsAtSymptomsDict symptom dict =
    Dict.get symptom dict
        |> Maybe.map ((<) 0)
        |> Maybe.withDefault False
