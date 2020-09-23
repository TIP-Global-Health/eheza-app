module Pages.AcuteIllnessEncounter.Utils exposing (..)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
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
        , HCRecommendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaRapidTestResult(..)
        , ReasonForNotIsolating(..)
        , SiteRecommendation(..)
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
import Pages.AcuteIllnessActivity.Model exposing (ExposureTask(..), LaboratoryTask(..), NextStepsTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (symptomsGeneralDangerSigns)
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


resolveNextStepsTasks : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> List NextStepsTask
resolveNextStepsTasks currentDate person diagnosis =
    let
        ( ageMonths0To2, ageMonths0To6, ageMonths2To60 ) =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonthss -> ( ageMonthss < 2, ageMonthss < 6, ageMonthss >= 2 && ageMonthss < 60 ))
                |> Maybe.withDefault ( False, False, False )

        expectTask task =
            case task of
                NextStepsIsolation ->
                    diagnosis == Just DiagnosisCovid19

                NextStepsCall114 ->
                    diagnosis == Just DiagnosisCovid19

                NextStepsMedicationDistribution ->
                    (diagnosis == Just DiagnosisMalariaUncomplicated && not ageMonths0To6)
                        || (diagnosis == Just DiagnosisGastrointestinalInfectionUncomplicated)
                        || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths2To60)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths2To60)

                NextStepsSendToHC ->
                    (diagnosis == Just DiagnosisMalariaUncomplicated && ageMonths0To6)
                        || (diagnosis == Just DiagnosisMalariaComplicated)
                        || (diagnosis == Just DiagnosisMalariaUncomplicatedAndPregnant)
                        || (diagnosis == Just DiagnosisGastrointestinalInfectionComplicated)
                        || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths0To2)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths0To2)
                        || (diagnosis == Just DiagnosisRespiratoryInfectionComplicated)
                        || (diagnosis == Just DiagnosisFeverOfUnknownOrigin)
                        || (diagnosis == Just DiagnosisUndeterminedMoreEvaluationNeeded)
    in
    [ NextStepsIsolation, NextStepsCall114, NextStepsMedicationDistribution, NextStepsSendToHC ]
        |> List.filter expectTask


expectActivity : NominalDate -> Person -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessActivity -> Bool
expectActivity currentDate person measurements diagnosis activity =
    case activity of
        AcuteIllnessLaboratory ->
            mandatoryActivitiesCompleted measurements
                && feverRecorded measurements

        AcuteIllnessNextSteps ->
            resolveNextStepByDiagnosis currentDate person diagnosis
                |> isJust

        _ ->
            True


activityCompleted : NominalDate -> Person -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessActivity -> Bool
activityCompleted currentDate person measurements diagnosis activity =
    case activity of
        AcuteIllnessSymptoms ->
            mandatoryActivityCompleted measurements AcuteIllnessSymptoms

        AcuteIllnessPhysicalExam ->
            mandatoryActivityCompleted measurements AcuteIllnessPhysicalExam

        AcuteIllnessPriorTreatment ->
            isJust measurements.treatmentReview

        AcuteIllnessLaboratory ->
            isJust measurements.malariaTesting

        AcuteIllnessExposure ->
            mandatoryActivityCompleted measurements AcuteIllnessExposure

        AcuteIllnessNextSteps ->
            case resolveNextStepsTasks currentDate person diagnosis of
                [ NextStepsIsolation, NextStepsCall114 ] ->
                    isJust measurements.isolation
                        && isJust measurements.hcContact

                [ NextStepsMedicationDistribution ] ->
                    isJust measurements.medicationDistribution

                [ NextStepsSendToHC ] ->
                    isJust measurements.sendToHC

                _ ->
                    False


{-| These are the activities that are mandatory, for us to come up with diagnosis.
Covid19 diagnosis is special, therefore, we assume here that Covid19 is negative.
-}
mandatoryActivitiesCompleted : AcuteIllnessMeasurements -> Bool
mandatoryActivitiesCompleted measurements =
    [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPhysicalExam ]
        |> List.all (mandatoryActivityCompleted measurements)


mandatoryActivityCompleted : AcuteIllnessMeasurements -> AcuteIllnessActivity -> Bool
mandatoryActivityCompleted measurements activity =
    case activity of
        AcuteIllnessSymptoms ->
            isJust measurements.symptomsGeneral
                && isJust measurements.symptomsRespiratory
                && isJust measurements.symptomsGI

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals
                && isJust measurements.acuteFindings

        AcuteIllnessExposure ->
            isJust measurements.travelHistory
                && isJust measurements.exposure

        _ ->
            False


resolveNextStepByDiagnosis : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> Maybe NextStepsTask
resolveNextStepByDiagnosis currentDate person maybeDiagnosis =
    maybeDiagnosis
        |> Maybe.andThen
            (\diagnosis ->
                case diagnosis of
                    DiagnosisCovid19 ->
                        Just NextStepsIsolation

                    DiagnosisMalariaUncomplicated ->
                        ageDependentUncomplicatedMalariaNextStep currentDate person

                    DiagnosisMalariaUncomplicatedAndPregnant ->
                        Just NextStepsSendToHC

                    DiagnosisMalariaComplicated ->
                        Just NextStepsSendToHC

                    DiagnosisGastrointestinalInfectionUncomplicated ->
                        Just NextStepsMedicationDistribution

                    DiagnosisGastrointestinalInfectionComplicated ->
                        Just NextStepsSendToHC

                    DiagnosisSimpleColdAndCough ->
                        ageDependentARINextStep currentDate person

                    DiagnosisRespiratoryInfectionUncomplicated ->
                        ageDependentARINextStep currentDate person

                    DiagnosisRespiratoryInfectionComplicated ->
                        Just NextStepsSendToHC

                    DiagnosisFeverOfUnknownOrigin ->
                        Just NextStepsSendToHC

                    DiagnosisUndeterminedMoreEvaluationNeeded ->
                        Just NextStepsSendToHC

                    NoAcuteIllnessDiagnosis ->
                        Nothing
            )


ageDependentUncomplicatedMalariaNextStep : NominalDate -> Person -> Maybe NextStepsTask
ageDependentUncomplicatedMalariaNextStep currentDate person =
    ageInMonths currentDate person
        |> Maybe.andThen
            (\ageMonths ->
                if ageMonths < 6 then
                    Just NextStepsSendToHC

                else
                    Just NextStepsMedicationDistribution
            )


ageDependentARINextStep : NominalDate -> Person -> Maybe NextStepsTask
ageDependentARINextStep currentDate person =
    ageInMonths currentDate person
        |> Maybe.andThen
            (\ageMonths ->
                if ageMonths < 2 then
                    Just NextStepsSendToHC

                else if ageMonths < 60 then
                    Just NextStepsMedicationDistribution

                else
                    Nothing
            )


resolveAcuteIllnessDiagnosis : NominalDate -> Person -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosis currentDate person measurements =
    let
        ( covid19ByCompleteSet, covid19ByPartialSet ) =
            covid19Diagnosed measurements
    in
    -- First we check for Covid19.
    if covid19ByCompleteSet then
        Just DiagnosisCovid19

    else
        resolveNonCovid19AcuteIllnessDiagnosis currentDate person covid19ByPartialSet measurements


covid19Diagnosed : AcuteIllnessMeasurements -> ( Bool, Bool )
covid19Diagnosed measurements =
    let
        countSigns measurement_ exclusion =
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
                                if EverySet.member exclusion set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        excludesGeneral =
            [ SymptomGeneralFever, NoSymptomsGeneral ] ++ symptomsGeneralDangerSigns

        generalSymptomsCount =
            countSymptoms measurements.symptomsGeneral .value excludesGeneral

        respiratorySymptomsCount =
            countRespiratorySymptoms measurements []

        giSymptomsCount =
            countGISymptoms measurements []

        totalSymptoms =
            generalSymptomsCount + respiratorySymptomsCount + giSymptomsCount

        symptomsIndicateCovid =
            if giSymptomsCount > 0 then
                respiratorySymptomsCount > 0

            else
                totalSymptoms > 1

        totalSigns =
            countSigns measurements.travelHistory NoTravelHistorySigns
                + countSigns measurements.exposure NoExposureSigns

        signsIndicateCovid =
            totalSigns > 0

        feverOnRecord =
            feverRecorded measurements

        rdtResult =
            malariaRapidTestResult measurements

        feverAndRdtNotPositive =
            feverOnRecord && isJust rdtResult && rdtResult /= Just RapidTestPositive
    in
    ( (signsIndicateCovid && symptomsIndicateCovid)
        || (signsIndicateCovid && feverOnRecord)
        || (not signsIndicateCovid && feverAndRdtNotPositive && respiratorySymptomsCount > 0)
        || (not signsIndicateCovid && feverAndRdtNotPositive && generalSymptomsCount > 1)
    , False
    )


resolveNonCovid19AcuteIllnessDiagnosis : NominalDate -> Person -> Bool -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveNonCovid19AcuteIllnessDiagnosis currentDate person covid19ByPartialSet measurements =
    -- Verify that we have enough data to make a decision on diagnosis.
    if mandatoryActivitiesCompleted measurements then
        if feverRecorded measurements then
            resolveAcuteIllnessDiagnosisByLaboratoryResults covid19ByPartialSet measurements

        else if respiratoryInfectionDangerSignsPresent measurements then
            Just DiagnosisRespiratoryInfectionComplicated

        else if gastrointestinalInfectionDangerSignsPresent measurements then
            Just DiagnosisGastrointestinalInfectionComplicated

        else if respiratoryInfectionSymptomsPresent measurements then
            if respiratoryRateElevated currentDate person measurements then
                Just DiagnosisRespiratoryInfectionUncomplicated

            else
                Just DiagnosisSimpleColdAndCough

        else if nonBloodyDiarrheaAtSymptoms measurements then
            -- Non Bloody Diarrhea is the only GI symptom that is diagnosed as Uncomplicated, when fever is  not recorded.
            Just DiagnosisGastrointestinalInfectionUncomplicated

        else if mildGastrointestinalInfectionSymptomsPresent measurements then
            Just DiagnosisUndeterminedMoreEvaluationNeeded

        else
            Nothing

    else
        Nothing


resolveAcuteIllnessDiagnosisByLaboratoryResults : Bool -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosisByLaboratoryResults covid19ByPartialSet measurements =
    malariaRapidTestResult measurements
        |> Maybe.andThen
            (\testResult ->
                case testResult of
                    RapidTestPositive ->
                        if malarialDangerSignsPresent measurements then
                            Just DiagnosisMalariaComplicated

                        else
                            Just DiagnosisMalariaUncomplicated

                    RapidTestPositiveAndPregnant ->
                        if malarialDangerSignsPresent measurements then
                            Just DiagnosisMalariaComplicated

                        else
                            Just DiagnosisMalariaUncomplicatedAndPregnant

                    _ ->
                        if respiratoryInfectionDangerSignsPresent measurements then
                            Just DiagnosisRespiratoryInfectionComplicated

                        else if nonBloodyDiarrheaAtSymptoms measurements then
                            -- Fever with Diarrhea is considered to be a complicated case.
                            Just DiagnosisGastrointestinalInfectionComplicated

                        else if covid19ByPartialSet then
                            Just DiagnosisCovid19

                        else
                            Just DiagnosisFeverOfUnknownOrigin
            )


countSymptoms : Maybe ( id, m ) -> (m -> Dict k v) -> List k -> Int
countSymptoms measurement geSymptomsListFunc exclusions =
    measurement
        |> Maybe.map
            (Tuple.second
                >> geSymptomsListFunc
                >> Dict.keys
                >> List.filter (\sign -> List.member sign exclusions |> not)
                >> List.length
            )
        |> Maybe.withDefault 0


countRespiratorySymptoms : AcuteIllnessMeasurements -> List SymptomsRespiratorySign -> Int
countRespiratorySymptoms measurements exclusions =
    countSymptoms measurements.symptomsRespiratory .value (NoSymptomsRespiratory :: exclusions)


countGISymptoms : AcuteIllnessMeasurements -> List SymptomsGISign -> Int
countGISymptoms measurements exclusions =
    countSymptoms measurements.symptomsGI (.value >> .signs) (NoSymptomsGI :: exclusions)


feverRecorded : AcuteIllnessMeasurements -> Bool
feverRecorded measurements =
    feverAtSymptoms measurements || isJust (feverAtPhysicalExam measurements)


feverAtSymptoms : AcuteIllnessMeasurements -> Bool
feverAtSymptoms measurements =
    measurements.symptomsGeneral
        |> Maybe.map (Tuple.second >> .value >> symptomAppearsAtSymptomsDict SymptomGeneralFever)
        |> Maybe.withDefault False


feverAtPhysicalExam : AcuteIllnessMeasurements -> Maybe Float
feverAtPhysicalExam measurements =
    measurements.vitals
        |> Maybe.andThen
            (\measurement ->
                let
                    bodyTemperature =
                        Tuple.second measurement
                            |> .value
                            |> .bodyTemperature
                in
                if bodyTemperature >= 37.5 then
                    Just bodyTemperature

                else
                    Nothing
            )


respiratoryRateElevated : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
respiratoryRateElevated currentDate person measurements =
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

            else if ageMonths < 144 then
                respiratoryRate >= 35

            else
                respiratoryRate >= 30
        )
        measurements.vitals
        (ageInMonths currentDate person)
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
                    vomiting && EverySet.member IntractableVomiting symptomsGISet

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


respiratoryInfectionSymptomsPresent : AcuteIllnessMeasurements -> Bool
respiratoryInfectionSymptomsPresent measurements =
    countRespiratorySymptoms measurements [ LossOfSmell, ShortnessOfBreath, StabbingChestPain ] > 0


respiratoryInfectionDangerSignsPresent : AcuteIllnessMeasurements -> Bool
respiratoryInfectionDangerSignsPresent measurements =
    Maybe.map2
        (\symptomsRespiratory acuteFindings ->
            let
                symptomsRespiratoryDict =
                    Tuple.second symptomsRespiratory |> .value

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value

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
            stridor
                || nasalFlaring
                || severeWheezing
                || subCostalRetractions
                || stabbingChestPain
                || shortnessOfBreath
        )
        measurements.symptomsRespiratory
        measurements.acuteFindings
        |> Maybe.withDefault False


gastrointestinalInfectionDangerSignsPresent : AcuteIllnessMeasurements -> Bool
gastrointestinalInfectionDangerSignsPresent measurements =
    Maybe.map
        (\symptomsGI ->
            let
                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs

                symptomsGISet =
                    Tuple.second symptomsGI |> .value |> .derivedSigns

                bloodyDiarrhea =
                    symptomAppearsAtSymptomsDict BloodyDiarrhea symptomsGIDict

                nonBloodyDiarrhea =
                    symptomAppearsAtSymptomsDict NonBloodyDiarrhea symptomsGIDict

                intractableVomiting =
                    symptomAppearsAtSymptomsDict Vomiting symptomsGIDict
                        && EverySet.member IntractableVomiting symptomsGISet
            in
            bloodyDiarrhea || (nonBloodyDiarrhea && intractableVomiting)
        )
        measurements.symptomsGI
        |> Maybe.withDefault False


mildGastrointestinalInfectionSymptomsPresent : AcuteIllnessMeasurements -> Bool
mildGastrointestinalInfectionSymptomsPresent measurements =
    Maybe.map
        (\symptomsGI ->
            let
                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs
            in
            symptomAppearsAtSymptomsDict SymptomGIAbdominalPain symptomsGIDict
                || symptomAppearsAtSymptomsDict Nausea symptomsGIDict
                || symptomAppearsAtSymptomsDict Vomiting symptomsGIDict
        )
        measurements.symptomsGI
        |> Maybe.withDefault False


nonBloodyDiarrheaAtSymptoms : AcuteIllnessMeasurements -> Bool
nonBloodyDiarrheaAtSymptoms measurements =
    measurements.symptomsGI
        |> Maybe.map (Tuple.second >> .value >> .signs >> symptomAppearsAtSymptomsDict NonBloodyDiarrhea)
        |> Maybe.withDefault False


vomitingAtSymptoms : AcuteIllnessMeasurements -> Bool
vomitingAtSymptoms measurements =
    measurements.symptomsGI
        |> Maybe.map (Tuple.second >> .value >> .signs >> symptomAppearsAtSymptomsDict Vomiting)
        |> Maybe.withDefault False


symptomAppearsAtSymptomsDict : a -> Dict a Int -> Bool
symptomAppearsAtSymptomsDict symptom dict =
    Dict.get symptom dict
        |> Maybe.map ((<) 0)
        |> Maybe.withDefault False


acuteIllnessDiagnosisToMaybe : AcuteIllnessDiagnosis -> Maybe AcuteIllnessDiagnosis
acuteIllnessDiagnosisToMaybe diagnosis =
    if diagnosis == NoAcuteIllnessDiagnosis then
        Nothing

    else
        Just diagnosis
