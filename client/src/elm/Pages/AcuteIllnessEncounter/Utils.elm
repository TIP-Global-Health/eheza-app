module Pages.AcuteIllnessEncounter.Utils exposing (..)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AcuteFindingsGeneralSign(..)
        , AcuteFindingsRespiratorySign(..)
        , AcuteIllnessDangerSign(..)
        , AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , Call114Sign(..)
        , ChildNutritionSign(..)
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecommendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaRapidTestResult(..)
        , MedicationDistributionSign(..)
        , MedicationNonAdministrationReason(..)
        , MedicationNonAdministrationSign(..)
        , MuacIndication(..)
        , ReasonForNotIsolating(..)
        , Recommendation114(..)
        , RecommendationSite(..)
        , SymptomsGIDerivedSign(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        )
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.AcuteIllnessActivity.Model exposing (ExposureTask(..), LaboratoryTask(..), NextStepsTask(..), PhysicalExamTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (expectPhysicalExamTask, symptomsGeneralDangerSigns)
import Pages.AcuteIllnessEncounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData currentDate id db =
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

        previousEncountersData =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> RemoteData.withDefault []

        assembled =
            RemoteData.map AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousEncountersData)
                |> RemoteData.andMap (Success Nothing)
                |> RemoteData.andMap (Success Nothing)

        ( currentDiagnosis, previousDiagnosis ) =
            if isJust diagnosisByCurrentEncounterMeasurements then
                ( diagnosisByCurrentEncounterMeasurements, currentByPreviousEncounters )

            else
                ( currentByPreviousEncounters, previousByPreviousEncounters )

        diagnosisByCurrentEncounterMeasurements =
            assembled
                |> RemoteData.toMaybe
                |> Maybe.andThen (resolveAcuteIllnessDiagnosis currentDate)
                |> Maybe.map (\diagnosis -> ( currentDate, diagnosis ))

        ( currentByPreviousEncounters, previousByPreviousEncounters ) =
            encounter
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.participant
                        >> getAcuteIllnessDiagnosisByPreviousEncounters id db
                    )
                |> Maybe.withDefault ( Nothing, Nothing )
    in
    assembled
        |> RemoteData.map (\data -> { data | diagnosis = currentDiagnosis, previousDiagnosis = previousDiagnosis })


generatePreviousMeasurements :
    AcuteIllnessEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> WebData (List AcuteIllnessEncounterData)
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
                                Just (Success measurements) ->
                                    Just (AcuteIllnessEncounterData encounterId encounter.startDate encounter.sequenceNumber encounter.diagnosis measurements)

                                _ ->
                                    Nothing
                    )
                >> List.sortWith compareAcuteIllnessEncounterDataAsc
            )


compareAcuteIllnessEncounterDataDesc :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncounterDataDesc data1 data2 =
    compareAcuteIllnessEncounterDataAsc data2 data1


compareAcuteIllnessEncounterDataAsc :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncounterDataAsc data1 data2 =
    case Gizra.NominalDate.compare data1.startDate data2.startDate of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare data1.sequenceNumber data2.sequenceNumber


getAcuteIllnessDiagnosisByPreviousEncounters :
    AcuteIllnessEncounterId
    -> ModelIndexedDb
    -> IndividualEncounterParticipantId
    -> ( Maybe ( NominalDate, AcuteIllnessDiagnosis ), Maybe ( NominalDate, AcuteIllnessDiagnosis ) )
getAcuteIllnessDiagnosisByPreviousEncounters currentEncounterId db participantId =
    let
        encountersWithDiagnosis =
            Dict.get participantId db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
                |> List.sortWith (\( _, e1 ) ( _, e2 ) -> compareAcuteIllnessEncounterDataDesc e1 e2)
                |> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter,
                        -- and those that do not have diagnosis set.
                        if encounterId == currentEncounterId || encounter.diagnosis == NoAcuteIllnessDiagnosis then
                            Nothing

                        else
                            Just ( encounter.startDate, encounter.diagnosis )
                    )
    in
    case encountersWithDiagnosis of
        [ current ] ->
            ( Just current, Nothing )

        [ current, previous ] ->
            ( Just current, Just previous )

        -- Since it's not possible to have more than 2 diagnosis,
        -- we get here when there're no diagnosis at all.
        _ ->
            ( Nothing, Nothing )


{-| Since there can be multiple encounters, resolved diagnosis is the one
that was set in most recent encounter.
-}
getAcuteIllnessDiagnosisForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe AcuteIllnessDiagnosis
getAcuteIllnessDiagnosisForParticipant db participantId =
    Dict.get participantId db.acuteIllnessEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.andThen
            (List.map Tuple.second
                >> List.sortWith compareAcuteIllnessEncounterDataDesc
                >> List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis)
                >> List.head
                >> Maybe.map .diagnosis
            )


resolveNextStepFirstEncounter : NominalDate -> AssembledData -> Maybe NextStepsTask
resolveNextStepFirstEncounter currentDate data =
    resolveNextStepsTasks currentDate True data
        |> List.head


resolveNextStepSubsequentEncounter : NominalDate -> AssembledData -> Maybe NextStepsTask
resolveNextStepSubsequentEncounter currentDate data =
    resolveNextStepsTasks currentDate False data
        |> List.head


resolveNextStepsTasks : NominalDate -> Bool -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate isFirstEncounter data =
    let
        diagnosis =
            Maybe.map Tuple.second data.diagnosis
    in
    if isFirstEncounter then
        -- The order is important. Do not change.
        [ NextStepsIsolation, NextStepsCall114, NextStepsContactHC, NextStepsMedicationDistribution, NextStepsSendToHC ]
            |> List.filter (expectNextStepsTaskFirstEncounter currentDate data.person diagnosis data.measurements)

    else if mandatoryActivitiesCompletedSubsequentVisit currentDate data then
        -- The order is important. Do not change.
        [ NextStepsContactHC, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsHealthEducation ]
            |> List.filter (expectNextStepsTaskSubsequentEncounter currentDate data.person diagnosis data.measurements)

    else
        []


expectNextStepsTaskFirstEncounter : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> NextStepsTask -> Bool
expectNextStepsTaskFirstEncounter currentDate person diagnosis measurements task =
    let
        ( ageMonths0To2, ageMonths0To6, ageMonths2To60 ) =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonthss -> ( ageMonthss < 2, ageMonthss < 6, ageMonthss >= 2 && ageMonthss < 60 ))
                |> Maybe.withDefault ( False, False, False )

        medicationPrescribed =
            (diagnosis == Just DiagnosisMalariaUncomplicated && not ageMonths0To6)
                || (diagnosis == Just DiagnosisGastrointestinalInfectionUncomplicated)
                || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths2To60)
                || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths2To60)
    in
    case task of
        NextStepsIsolation ->
            diagnosis == Just DiagnosisCovid19

        NextStepsCall114 ->
            diagnosis == Just DiagnosisCovid19

        NextStepsContactHC ->
            diagnosis == Just DiagnosisCovid19 && isJust measurements.call114 && (not <| talkedTo114 measurements)

        NextStepsMedicationDistribution ->
            medicationPrescribed

        NextStepsSendToHC ->
            sendToHCByMalariaTesting ageMonths0To6 diagnosis
                || (diagnosis == Just DiagnosisGastrointestinalInfectionComplicated)
                || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths0To2)
                || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths0To2)
                || (diagnosis == Just DiagnosisRespiratoryInfectionComplicated)
                || (diagnosis == Just DiagnosisFeverOfUnknownOrigin)
                || (diagnosis == Just DiagnosisUndeterminedMoreEvaluationNeeded)
                -- Medication was perscribed, but it's out of stock, or patient is alergic.
                || (medicationPrescribed && sendToHCDueToMedicationNonAdministration measurements)

        NextStepsHealthEducation ->
            False


{-| Send patient to health center if patient is alergic to any of prescribed medications,
or, if any of prescribed medications is out of stock.
-}
sendToHCDueToMedicationNonAdministration : AcuteIllnessMeasurements -> Bool
sendToHCDueToMedicationNonAdministration measurements =
    resolveMedicationsNonAdministrationReasons measurements
        |> List.filter
            (\( _, reason ) ->
                reason == NonAdministrationLackOfStock || reason == NonAdministrationKnownAllergy
            )
        |> List.isEmpty
        |> not


resolveMedicationsNonAdministrationReasons : AcuteIllnessMeasurements -> List ( MedicationDistributionSign, MedicationNonAdministrationReason )
resolveMedicationsNonAdministrationReasons measurements =
    let
        nonAdministrationSigns =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> .nonAdministrationSigns
                    >> EverySet.toList
                )
                measurements.medicationDistribution
    in
    nonAdministrationSigns
        |> Maybe.map
            (List.filterMap
                (\sign ->
                    case sign of
                        MedicationAmoxicillin reason ->
                            Just ( Amoxicillin, reason )

                        MedicationCoartem reason ->
                            Just ( Coartem, reason )

                        MedicationORS reason ->
                            Just ( ORS, reason )

                        MedicationZinc reason ->
                            Just ( Zinc, reason )

                        NoMedicationNonAdministrationSigns ->
                            Nothing
                )
            )
        |> Maybe.withDefault []


expectNextStepsTaskSubsequentEncounter : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> NextStepsTask -> Bool
expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements task =
    let
        malariaDiagnosedAtCurrentEncounter =
            malariaRapidTestResult measurements == Just RapidTestPositive

        ageMonths0To6 =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonthss -> ageMonthss < 6)
                |> Maybe.withDefault False
    in
    case task of
        NextStepsMedicationDistribution ->
            malariaDiagnosedAtCurrentEncounter
                && (not <| sendToHCByMalariaTesting ageMonths0To6 diagnosis)

        NextStepsSendToHC ->
            if malariaDiagnosedAtCurrentEncounter then
                sendToHCByMalariaTesting ageMonths0To6 diagnosis
                    || -- Medication was perscribed, but it's out of stock, or patient is alergic.
                       sendToHCDueToMedicationNonAdministration measurements

            else
                -- No improvement, without danger signs.
                noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements
                    || -- No improvement, with danger signs, and diagnosis is not Covid19.
                       (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements && diagnosis /= Just DiagnosisCovid19)
                    || -- No improvement, with danger signs, diagnosis is Covid19, and HC recomended to send patient over.
                       (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements
                            && (diagnosis == Just DiagnosisCovid19)
                            && healthCenterRecommendedToCome measurements
                       )

        NextStepsContactHC ->
            not malariaDiagnosedAtCurrentEncounter
                && -- No improvement, with danger signs, and diagnosis is Covid19.
                   (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements && diagnosis == Just DiagnosisCovid19)

        NextStepsHealthEducation ->
            not malariaDiagnosedAtCurrentEncounter

        _ ->
            False


talkedTo114 : AcuteIllnessMeasurements -> Bool
talkedTo114 measurements =
    measurements.call114
        |> Maybe.map (Tuple.second >> .value >> .signs >> EverySet.member Call114)
        |> Maybe.withDefault False


healthCenterRecommendedToCome : AcuteIllnessMeasurements -> Bool
healthCenterRecommendedToCome measurements =
    measurements.hcContact
        |> Maybe.map (Tuple.second >> .value >> .recommendations >> EverySet.member ComeToHealthCenter)
        |> Maybe.withDefault False


sendToHCByMalariaTesting : Bool -> Maybe AcuteIllnessDiagnosis -> Bool
sendToHCByMalariaTesting ageMonths0To6 diagnosis =
    (diagnosis == Just DiagnosisMalariaUncomplicated && ageMonths0To6)
        || (diagnosis == Just DiagnosisMalariaComplicated)
        || (diagnosis == Just DiagnosisMalariaUncomplicatedAndPregnant)


noImprovementOnSubsequentVisit : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisit currentDate person measurements =
    noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements
        || noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements


noImprovementOnSubsequentVisitWithoutDangerSigns : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements =
    (not <| dangerSignPresentOnSubsequentVisit measurements)
        && (conditionNotImprovingOnSubsequentVisit measurements
                || sendToHCOnSubsequentVisitByVitals currentDate person measurements
                || sendToHCOnSubsequentVisitByMuac measurements
                || sendToHCOnSubsequentVisitByNutrition measurements
           )


noImprovementOnSubsequentVisitWithDangerSigns : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements =
    dangerSignPresentOnSubsequentVisit measurements


conditionNotImprovingOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
conditionNotImprovingOnSubsequentVisit measurements =
    measurements.dangerSigns
        |> Maybe.map
            (Tuple.second
                >> .value
                >> EverySet.member DangerSignConditionNotImproving
            )
        |> Maybe.withDefault False


dangerSignPresentOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
dangerSignPresentOnSubsequentVisit measurements =
    measurements.dangerSigns
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (EverySet.toList
                        >> (\signs ->
                                List.any (\sign -> List.member sign signs)
                                    [ DangerSignUnableDrinkSuck
                                    , DangerSignVomiting
                                    , DangerSignConvulsions
                                    , DangerSignLethargyUnconsciousness
                                    , DangerSignRespiratoryDistress
                                    , DangerSignSpontaneousBleeding
                                    , DangerSignBloodyDiarrhea
                                    , DangerSignNewSkinRash
                                    ]
                           )
                   )
            )
        |> Maybe.withDefault False


sendToHCOnSubsequentVisitByVitals : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByVitals currentDate person measurements =
    feverRecorded measurements
        || respiratoryRateElevated currentDate person measurements


sendToHCOnSubsequentVisitByMuac : AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByMuac measurements =
    muacRedOnSubsequentVisit measurements


sendToHCOnSubsequentVisitByNutrition : AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByNutrition measurements =
    measurements.nutrition
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (EverySet.toList
                        >> (\signs ->
                                -- Any of 4 signs requires sending patient to HC.
                                List.any (\sign -> List.member sign signs) [ AbdominalDistension, Apathy, Edema, PoorAppetite ]
                                    -- Existance of both signs togetehr requires sending patient to HC.
                                    || List.all (\sign -> List.member sign signs) [ BrittleHair, DrySkin ]
                           )
                   )
            )
        |> Maybe.withDefault False


muacRedOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
muacRedOnSubsequentVisit measurements =
    measurements.muac
        |> Maybe.map
            (Tuple.second
                >> .value
                >> muacIndication
                >> (==) MuacRed
            )
        |> Maybe.withDefault False


expectActivity : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
expectActivity currentDate isFirstEncounter data activity =
    case activity of
        AcuteIllnessLaboratory ->
            if isFirstEncounter then
                mandatoryActivitiesCompletedFirstEncounter currentDate data
                    && feverRecorded data.measurements

            else
                -- If fever is recorded on current encounter, and patient did not
                -- test positive to Malaria during previous encounters,
                -- we want patient to take Malaria test.
                feverRecorded data.measurements
                    && (data.previousEncountersData
                            |> List.filter
                                (.measurements
                                    >> .malariaTesting
                                    >> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> (\testResult -> testResult == RapidTestPositive || testResult == RapidTestPositiveAndPregnant)
                                        )
                                    >> Maybe.withDefault False
                                )
                            |> List.isEmpty
                       )

        AcuteIllnessOngoingTreatment ->
            if isFirstEncounter then
                False

            else
                -- Show activity, if medication was perscribed at any of previous encounters.
                data.previousEncountersData
                    |> List.filterMap
                        (.measurements
                            >> .medicationDistribution
                            >> Maybe.andThen
                                (Tuple.second
                                    >> .value
                                    >> .distributionSigns
                                    >> (\medications ->
                                            if
                                                (medications /= EverySet.singleton NoMedicationDistributionSigns)
                                                    -- Lemon juice does not count as a medication.
                                                    && (medications /= EverySet.singleton LemonJuiceOrHoney)
                                            then
                                                Just True

                                            else
                                                Nothing
                                       )
                                )
                        )
                    |> List.isEmpty
                    |> not

        AcuteIllnessNextSteps ->
            resolveNextStepsTasks currentDate isFirstEncounter data
                |> List.isEmpty
                |> not

        _ ->
            True


activityCompleted : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
activityCompleted currentDate isFirstEncounter data activity =
    let
        person =
            data.person

        measurements =
            data.measurements

        diagnosis =
            Maybe.map Tuple.second data.diagnosis
    in
    case activity of
        AcuteIllnessSymptoms ->
            mandatoryActivityCompletedFirstEncounter currentDate data AcuteIllnessSymptoms

        AcuteIllnessPhysicalExam ->
            if isFirstEncounter then
                mandatoryActivityCompletedFirstEncounter currentDate data AcuteIllnessPhysicalExam

            else
                mandatoryActivityCompletedSubsequentVisit currentDate data AcuteIllnessPhysicalExam

        AcuteIllnessPriorTreatment ->
            isJust measurements.treatmentReview

        AcuteIllnessLaboratory ->
            if isFirstEncounter then
                mandatoryActivityCompletedFirstEncounter currentDate data AcuteIllnessLaboratory

            else
                mandatoryActivityCompletedSubsequentVisit currentDate data AcuteIllnessLaboratory

        AcuteIllnessExposure ->
            mandatoryActivityCompletedFirstEncounter currentDate data AcuteIllnessExposure

        AcuteIllnessNextSteps ->
            let
                nextStepsTasks =
                    resolveNextStepsTasks currentDate isFirstEncounter data
            in
            if isFirstEncounter then
                case nextStepsTasks of
                    [ NextStepsIsolation, NextStepsCall114 ] ->
                        isJust measurements.isolation && isJust measurements.call114

                    [ NextStepsIsolation, NextStepsCall114, NextStepsContactHC ] ->
                        isJust measurements.isolation && isJust measurements.call114 && isJust measurements.hcContact

                    [ NextStepsMedicationDistribution ] ->
                        isJust measurements.medicationDistribution

                    -- When medication was prescribed, but it is out
                    -- of stock, or patient is alergic.
                    [ NextStepsMedicationDistribution, NextStepsSendToHC ] ->
                        isJust measurements.medicationDistribution && isJust measurements.sendToHC

                    [ NextStepsSendToHC ] ->
                        isJust measurements.sendToHC

                    _ ->
                        False

            else
                case nextStepsTasks of
                    -- Improving, without danger signs present.
                    [ NextStepsHealthEducation ] ->
                        isJust measurements.healthEducation

                    -- Not improving, without danger signs present.
                    [ NextStepsSendToHC, NextStepsHealthEducation ] ->
                        isJust measurements.sendToHC && isJust measurements.healthEducation

                    -- Not improving, with danger signs, and not instructed to send patient to health center.
                    [ NextStepsContactHC, NextStepsHealthEducation ] ->
                        isJust measurements.hcContact && isJust measurements.healthEducation

                    -- Not improving, with danger signs, and instructed to send patient to health center.
                    [ NextStepsContactHC, NextStepsSendToHC, NextStepsHealthEducation ] ->
                        isJust measurements.hcContact && isJust measurements.sendToHC && isJust measurements.healthEducation

                    -- Uncomplicated malarial for adult.
                    [ NextStepsMedicationDistribution ] ->
                        isJust measurements.medicationDistribution

                    -- Uncomplicated malarial for adult, when medicine is out
                    -- of stock, or patient is alergic.
                    [ NextStepsMedicationDistribution, NextStepsSendToHC ] ->
                        isJust measurements.medicationDistribution && isJust measurements.sendToHC

                    -- Other cases of malaria.
                    [ NextStepsSendToHC ] ->
                        isJust measurements.sendToHC

                    _ ->
                        False

        AcuteIllnessDangerSigns ->
            mandatoryActivityCompletedSubsequentVisit currentDate data AcuteIllnessDangerSigns

        AcuteIllnessOngoingTreatment ->
            mandatoryActivityCompletedSubsequentVisit currentDate data AcuteIllnessOngoingTreatment


{-| These are the activities that are mandatory for us to come up with diagnosis during first encounter.
Covid19 diagnosis is special, therefore, we assume here that Covid19 is negative.
-}
mandatoryActivitiesCompletedFirstEncounter : NominalDate -> AssembledData -> Bool
mandatoryActivitiesCompletedFirstEncounter currentDate data =
    [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPhysicalExam ]
        |> List.all (mandatoryActivityCompletedFirstEncounter currentDate data)


mandatoryActivityCompletedFirstEncounter : NominalDate -> AssembledData -> AcuteIllnessActivity -> Bool
mandatoryActivityCompletedFirstEncounter currentDate data activity =
    let
        person =
            data.person

        measurements =
            data.measurements
    in
    case activity of
        AcuteIllnessSymptoms ->
            isJust measurements.symptomsGeneral
                && isJust measurements.symptomsRespiratory
                && isJust measurements.symptomsGI

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals
                && isJust measurements.acuteFindings
                && ((not <| expectPhysicalExamTask currentDate person False PhysicalExamMuac) || isJust measurements.muac)
                && ((not <| expectPhysicalExamTask currentDate person False PhysicalExamNutrition) || isJust measurements.nutrition)

        AcuteIllnessExposure ->
            isJust measurements.travelHistory
                && isJust measurements.exposure

        AcuteIllnessLaboratory ->
            let
                rdtExecuted =
                    malariaRapidTestExecuted measurements
            in
            (not <| expectActivity currentDate True data AcuteIllnessLaboratory)
                || (isJust measurements.malariaTesting
                        && (not rdtExecuted || (rdtExecuted && isJust measurements.barcodeScan))
                   )

        _ ->
            False


{-| These are the activities that are mandatory for us to come up with next steps during subsequent encounter.
-}
mandatoryActivitiesCompletedSubsequentVisit : NominalDate -> AssembledData -> Bool
mandatoryActivitiesCompletedSubsequentVisit currentDate data =
    [ AcuteIllnessDangerSigns, AcuteIllnessPhysicalExam, AcuteIllnessOngoingTreatment, AcuteIllnessLaboratory ]
        |> List.all (mandatoryActivityCompletedSubsequentVisit currentDate data)


mandatoryActivityCompletedSubsequentVisit : NominalDate -> AssembledData -> AcuteIllnessActivity -> Bool
mandatoryActivityCompletedSubsequentVisit currentDate data activity =
    let
        person =
            data.person

        measurements =
            data.measurements
    in
    case activity of
        AcuteIllnessDangerSigns ->
            isJust measurements.dangerSigns

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals
                && ((not <| expectPhysicalExamTask currentDate person False PhysicalExamMuac) || isJust measurements.muac)
                && ((not <| expectPhysicalExamTask currentDate person False PhysicalExamNutrition) || isJust measurements.nutrition)
                && ((not <| expectPhysicalExamTask currentDate person False PhysicalExamAcuteFindings) || isJust measurements.acuteFindings)

        AcuteIllnessOngoingTreatment ->
            (not <| expectActivity currentDate False data AcuteIllnessOngoingTreatment)
                || isJust measurements.treatmentOngoing

        AcuteIllnessLaboratory ->
            let
                rdtExecuted =
                    malariaRapidTestExecuted measurements
            in
            (not <| expectActivity currentDate False data AcuteIllnessLaboratory)
                || (isJust measurements.malariaTesting
                        && (not rdtExecuted || (rdtExecuted && isJust measurements.barcodeScan))
                   )

        _ ->
            False


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


resolveAcuteIllnessDiagnosis : NominalDate -> AssembledData -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosis currentDate data =
    let
        isFirstEncounter =
            List.isEmpty data.previousEncountersData

        measurements =
            data.measurements
    in
    if isFirstEncounter then
        let
            ( covid19ByCompleteSet, covid19ByPartialSet ) =
                covid19Diagnosed data.measurements
        in
        -- First we check for Covid19.
        if covid19ByCompleteSet then
            Just DiagnosisCovid19

        else
            resolveNonCovid19AcuteIllnessDiagnosis currentDate covid19ByPartialSet data

    else if malariaRapidTestExecuted measurements && isNothing measurements.barcodeScan then
        -- We do not determine a diagnosis until the Malaria test kit barcode is scanned.
        Nothing

    else
        resolveMalariaDiagnosisForPositiveRDT data.measurements


resolveMalariaDiagnosisForPositiveRDT : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveMalariaDiagnosisForPositiveRDT measurements =
    malariaRapidTestResult measurements
        |> Maybe.andThen
            (\testResult ->
                case testResult of
                    RapidTestPositive ->
                        if dangerSignPresentOnSubsequentVisit measurements then
                            Just DiagnosisMalariaComplicated

                        else
                            Just DiagnosisMalariaUncomplicated

                    RapidTestPositiveAndPregnant ->
                        if dangerSignPresentOnSubsequentVisit measurements then
                            Just DiagnosisMalariaComplicated

                        else
                            Just DiagnosisMalariaUncomplicatedAndPregnant

                    _ ->
                        Nothing
            )


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


resolveNonCovid19AcuteIllnessDiagnosis : NominalDate -> Bool -> AssembledData -> Maybe AcuteIllnessDiagnosis
resolveNonCovid19AcuteIllnessDiagnosis currentDate covid19ByPartialSet data =
    let
        person =
            data.person

        measurements =
            data.measurements
    in
    -- Verify that we have enough data to make a decision on diagnosis.
    if mandatoryActivitiesCompletedFirstEncounter currentDate data then
        if feverRecorded measurements then
            resolveAcuteIllnessDiagnosisByLaboratoryResults covid19ByPartialSet measurements

        else if respiratoryInfectionDangerSignsPresent measurements then
            Just DiagnosisRespiratoryInfectionComplicated

        else if gastrointestinalInfectionDangerSignsPresent False measurements then
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
    if malariaRapidTestExecuted measurements then
        if isNothing measurements.barcodeScan then
            -- We do not determine a diagnosis until the Malaria test kit barcode is scanned.
            Nothing

        else
            resolveMalariaDiagnosisForPositiveRDT measurements
                |> Maybe.Extra.orElse
                    (if respiratoryInfectionDangerSignsPresent measurements then
                        Just DiagnosisRespiratoryInfectionComplicated

                     else if gastrointestinalInfectionDangerSignsPresent True measurements then
                        -- Fever with Diarrhea is considered to be a complicated case.
                        Just DiagnosisGastrointestinalInfectionComplicated

                     else if covid19ByPartialSet then
                        Just DiagnosisCovid19

                     else
                        Just DiagnosisFeverOfUnknownOrigin
                    )

    else
        Nothing


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
    Maybe.map
        (\measurement ->
            let
                maybeAgeMonths =
                    ageInMonths currentDate person

                respiratoryRate =
                    Tuple.second measurement
                        |> .value
                        |> .respiratoryRate
            in
            respiratoryRateElevatedForAge maybeAgeMonths respiratoryRate
        )
        measurements.vitals
        |> Maybe.withDefault False


respiratoryRateElevatedForAge : Maybe Int -> Int -> Bool
respiratoryRateElevatedForAge maybeAgeMonths rate =
    maybeAgeMonths
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 12 then
                    rate >= 50

                else if ageMonths < 60 then
                    rate >= 40

                else
                    rate > 30
            )
        |> Maybe.withDefault False


malariaRapidTestResult : AcuteIllnessMeasurements -> Maybe MalariaRapidTestResult
malariaRapidTestResult measurements =
    measurements.malariaTesting
        |> Maybe.map (Tuple.second >> .value)


malariaRapidTestExecuted : AcuteIllnessMeasurements -> Bool
malariaRapidTestExecuted measurements =
    let
        rdtResult =
            malariaRapidTestResult measurements
    in
    isJust rdtResult && rdtResult /= Just RapidTestUnableToRun


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


gastrointestinalInfectionDangerSignsPresent : Bool -> AcuteIllnessMeasurements -> Bool
gastrointestinalInfectionDangerSignsPresent fever measurements =
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
            if fever then
                bloodyDiarrhea || nonBloodyDiarrhea

            else
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
