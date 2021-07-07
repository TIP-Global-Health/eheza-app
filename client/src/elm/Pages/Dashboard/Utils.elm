module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model exposing (AcuteIllnessDataItem, AcuteIllnessEncounterDataItem, AssembledData, DashboardStats, PrenatalDataItem)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model
    exposing
        ( Call114Sign(..)
        , DangerSign(..)
        , FollowUpMeasurements
        , HCContactSign(..)
        , HCRecommendation(..)
        , IsolationSign(..)
        , Recommendation114(..)
        , SendToHCSign(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (filterVillageResidents, generateAcuteIllnessFollowUps, generateNutritionFollowUps, generatePrenatalFollowUps)
import Pages.GlobalCaseManagement.View exposing (generateAcuteIllnessFollowUpEntries, generateNutritionFollowUpEntries, generatePrenatalFollowUpEntries)
import RemoteData
import Translate exposing (Language)


filterProgramTypeToString : FilterProgramType -> String
filterProgramTypeToString filterProgramType =
    case filterProgramType of
        FilterAllPrograms ->
            "all"

        FilterProgramAchi ->
            "achi"

        FilterProgramFbf ->
            "fbf"

        FilterProgramPmtct ->
            "pmtct"

        FilterProgramSorwathe ->
            "sorwathe"

        FilterProgramCommunity ->
            "community"


filterProgramTypeFromString : String -> Maybe FilterProgramType
filterProgramTypeFromString string =
    case string of
        "all" ->
            Just FilterAllPrograms

        "achi" ->
            Just FilterProgramAchi

        "fbf" ->
            Just FilterProgramFbf

        "pmtct" ->
            Just FilterProgramPmtct

        "sorwathe" ->
            Just FilterProgramSorwathe

        "community" ->
            Just FilterProgramCommunity

        _ ->
            Nothing


generateAssembledData : HealthCenterId -> Maybe VillageId -> DashboardStats -> ModelIndexedDb -> AssembledData
generateAssembledData healthCenterId villageId stats db =
    { stats = stats
    , acuteIllnessData = generateFilteredAcuteIllnessData villageId stats
    , prenatalData = generateFilteredPrenatalData villageId stats
    , caseManagementData =
        Dict.get healthCenterId db.followUpMeasurements
            |> Maybe.andThen RemoteData.toMaybe
    }


generateFilteredPrenatalData : Maybe VillageId -> DashboardStats -> List PrenatalDataItem
generateFilteredPrenatalData maybeVillageId stats =
    maybeVillageId
        |> Maybe.andThen
            (\villageId -> Dict.get villageId stats.villagesWithResidents)
        |> Maybe.map
            (\residents -> List.filter (\item -> List.member item.identifier residents) stats.prenatalData)
        |> Maybe.withDefault []


generateFilteredAcuteIllnessData : Maybe VillageId -> DashboardStats -> List AcuteIllnessDataItem
generateFilteredAcuteIllnessData maybeVillageId stats =
    maybeVillageId
        |> Maybe.andThen
            (\villageId -> Dict.get villageId stats.villagesWithResidents)
        |> Maybe.map
            (\residents -> List.filter (\item -> List.member item.identifier residents) stats.acuteIllnessData)
        |> Maybe.withDefault []



--
-- Acute illness - Overview functions.
--


getAcuteIllnessEncountersForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem
getAcuteIllnessEncountersForSelectedMonth selectedDate itemsList =
    List.map .encounters itemsList
        |> List.concat
        |> List.filter (.startDate >> withinSelectedMonth selectedDate)


countAcuteIllnessAssesments : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessAssesments encounters =
    -- Count number of encounters that occured during selected month.
    List.length encounters


countAcuteIllnessDiagnosedCases : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessDiagnosedCases encounters =
    List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) encounters
        |> List.length


countAcuteIllnessCasesByTreatmentApproach : List AcuteIllnessEncounterDataItem -> ( Int, Int )
countAcuteIllnessCasesByTreatmentApproach encounters =
    let
        diagnosedEncounters =
            List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) encounters

        sentToHC =
            List.filter wasSentToHCByDiagnosis diagnosedEncounters

        managedAtHome =
            List.filter wasManagedAtHomeByDiagnosis diagnosedEncounters
    in
    ( List.length sentToHC, List.length managedAtHome )


{-| There's a difference betweeen non Covid and Covid cases, when making
a decision if to send patient to health center.
Covid case has a specific set of parameters, while non Covid has a simple logic -
only those that Yes answered to quesiton about patien being refered to HC.
-}
wasSentToHCByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasSentToHCByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19 ->
            let
                sentToHCBy114 =
                    EverySet.member Call114 encounter.call114Signs
                        && EverySet.member SendToHealthCenter encounter.recommendation114

                sentToHCByHC =
                    EverySet.member ContactedHealthCenter encounter.hcContactSigns
                        && EverySet.member ComeToHealthCenter encounter.hcRecommendation
            in
            sentToHCBy114 || sentToHCByHC

        -- All others, but it must exclude NoAcuteIllnessDiagnosis - invoking function
        -- should be taking care of this.
        _ ->
            -- All that were refered sent to HC.
            EverySet.member ReferToHealthCenter encounter.sendToHCSigns


{-| There's a difference betweeen non Covid and Covid cases, when making
a decision if to manage illness at home.
Covid case has a specific set of parameters, while non Covid has a simple logic -
if patient was not sent to HC, then it was managed at home.
-}
wasManagedAtHomeByDiagnosis : AcuteIllnessEncounterDataItem -> Bool
wasManagedAtHomeByDiagnosis encounter =
    case encounter.diagnosis of
        DiagnosisCovid19 ->
            -- HC was contacted, and it suggested home isolation
            -- or CHW monitoring.
            EverySet.member ContactedHealthCenter encounter.hcContactSigns
                && (EverySet.member HomeIsolation encounter.hcRecommendation
                        || EverySet.member ChwMonitoring encounter.hcRecommendation
                   )

        -- All others, but it must exclude NoAcuteIllnessDiagnosis - invoking function
        -- should be taking care of this.
        _ ->
            -- All that were not refered to HC are managed at home.
            not <| wasSentToHCByDiagnosis encounter


countAcuteIllnessCasesByPossibleDiagnosises : List AcuteIllnessDiagnosis -> Bool -> List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessCasesByPossibleDiagnosises possible whenFeverRecorded encounters =
    List.filter
        (\encounter ->
            let
                feverFilter =
                    if whenFeverRecorded then
                        encounter.feverRecorded

                    else
                        True
            in
            List.member encounter.diagnosis possible
                && feverFilter
        )
        encounters
        |> List.length



--
-- Acute illness - COVID functions.
--


countDiagnosedWithCovidCallsTo114 : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidCallsTo114 encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Covid19 diagnosis,
            -- and there was a call to 114.
            (encounter.diagnosis == DiagnosisCovid19)
                && EverySet.member Call114 encounter.call114Signs
        )
        encounters
        |> List.length


countDiagnosedWithCovidSentToHC : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidSentToHC encounters =
    -- Encounters which has produced Covid19 diagnosis,
    -- and patient was sent to health center.
    List.filter (.diagnosis >> (==) DiagnosisCovid19) encounters
        |> List.filter wasSentToHCByDiagnosis
        |> List.length


countDiagnosedWithCovidManagedAtHome : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithCovidManagedAtHome encounters =
    -- Encounter which has produced Covid19 diagnosis,
    -- and it was decided to manage illness at home.
    List.filter (.diagnosis >> (==) DiagnosisCovid19) encounters
        |> List.filter wasManagedAtHomeByDiagnosis
        |> List.length



--
-- Acute illness - Malaria functions.
--


countDiagnosedWithMalaria : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithMalaria encounters =
    List.filter
        (\encounter ->
            List.member encounter.diagnosis
                [ DiagnosisMalariaComplicated
                , DiagnosisMalariaUncomplicated
                , DiagnosisMalariaUncomplicatedAndPregnant
                ]
        )
        encounters
        |> List.length


countUncomplicatedMalariaManagedByChw : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedMalariaManagedByChw encounters =
    List.filter
        (\encounter ->
            -- Enconter which has produced Uncomplicated Malaria diagnosis,
            -- and patient was not sent to health center.
            (encounter.diagnosis == DiagnosisMalariaUncomplicated)
                && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
        )
        encounters
        |> List.length


countUncomplicatedMalariaAndPregnantSentToHC : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedMalariaAndPregnantSentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Uncomplicated Malaria and Pregnant
            -- diagnosis, and patient was sent to health center.
            (encounter.diagnosis == DiagnosisMalariaUncomplicatedAndPregnant)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countComplicatedMalariaSentToHC : List AcuteIllnessEncounterDataItem -> Int
countComplicatedMalariaSentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Comlpicated Malaria diagnosis,
            -- and patient was sent to health center.
            (encounter.diagnosis == DiagnosisMalariaComplicated)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countResolvedMalariaCasesForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countResolvedMalariaCasesForSelectedMonth selectedDate itemsList =
    List.filter
        (\item ->
            case item.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- and had a Malaria diagnosis.
                    withinSelectedMonth selectedDate dateConcluded
                        && List.member item.diagnosis
                            [ DiagnosisMalariaComplicated
                            , DiagnosisMalariaUncomplicated
                            , DiagnosisMalariaUncomplicatedAndPregnant
                            ]
        )
        itemsList
        |> List.length



--
-- Acute illness - Gastro functions.
--


countDiagnosedWithGI : List AcuteIllnessEncounterDataItem -> Int
countDiagnosedWithGI encounters =
    List.filter
        (\encounter ->
            List.member encounter.diagnosis
                [ DiagnosisGastrointestinalInfectionComplicated
                , DiagnosisGastrointestinalInfectionUncomplicated
                ]
        )
        encounters
        |> List.length


countUncomplicatedGIManagedByChw : List AcuteIllnessEncounterDataItem -> Int
countUncomplicatedGIManagedByChw encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Uncomlicated GI diagnosis,
            -- and patient was not sent to health center.
            (encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated)
                && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
        )
        encounters
        |> List.length


countComplicatedGISentToHC : List AcuteIllnessEncounterDataItem -> Int
countComplicatedGISentToHC encounters =
    List.filter
        (\encounter ->
            -- Encounter which has produced Comlicated GI diagnosis,
            -- and patient was sent to health center.
            (encounter.diagnosis == DiagnosisGastrointestinalInfectionComplicated)
                && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
        )
        encounters
        |> List.length


countResolvedGICasesForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countResolvedGICasesForSelectedMonth selectedDate itemsList =
    List.filter
        (\item ->
            case item.dateConcluded of
                Nothing ->
                    False

                Just dateConcluded ->
                    -- Illness that was resolved at selected month,
                    -- and had a GI diagnosis.
                    withinSelectedMonth selectedDate dateConcluded
                        && List.member item.diagnosis
                            [ DiagnosisGastrointestinalInfectionComplicated
                            , DiagnosisGastrointestinalInfectionUncomplicated
                            ]
        )
        itemsList
        |> List.length



--
-- ANC functions.
--


countNewlyIdentifiedPregananciesForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countNewlyIdentifiedPregananciesForSelectedMonth selectedDate itemsList =
    itemsList
        |> List.filter (.created >> withinSelectedMonth selectedDate)
        |> List.length


countCurrentlyPregnantForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> Int
countCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList
        |> List.length


getCurrentlyPregnantForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> List PrenatalDataItem
getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month selectedDate

        dateFirstDayOfNextMonth =
            Date.ceiling Date.Month selectedDate
    in
    itemsList
        |> List.filter
            (\item ->
                let
                    -- Pregnancy was tracked during current month, or before.
                    createdDateFilter =
                        Date.compare item.created dateFirstDayOfNextMonth == LT

                    -- Expected date exists, and is set to 3 weeks or less,
                    -- before the beggining of the range.
                    expectedDateConcludedFilter =
                        item.expectedDateConcluded
                            |> Maybe.map
                                (\expectedDateConcluded ->
                                    Date.diff Date.Weeks expectedDateConcluded dateFirstDayOfSelectedMonth <= 3
                                )
                            |> Maybe.withDefault False

                    -- No date concluded, or it's set within month range, or after that.
                    actualDateConcludedFilter =
                        case item.dateConcluded of
                            Just dateConcluded ->
                                let
                                    compareResult =
                                        Date.compare dateFirstDayOfSelectedMonth dateConcluded
                                in
                                compareResult == LT || compareResult == EQ

                            Nothing ->
                                True
                in
                createdDateFilter && expectedDateConcludedFilter && actualDateConcludedFilter
            )


countCurrentlyPregnantWithDangerSignsForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> Int
countCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList
        |> List.length


getCurrentlyPregnantWithDangerSignsForSelectedMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> List PrenatalDataItem
getCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate itemsList =
    getCurrentlyPregnantForSelectedMonth currentDate selectedDate itemsList
        |> List.filter
            (.encounters
                >> List.any
                    (\encounter ->
                        -- Active pregnancy that got an encounter at
                        -- selected month, where danger signs where recorded.
                        withinSelectedMonth selectedDate encounter.startDate
                            && (not <| EverySet.isEmpty encounter.dangerSigns)
                            && (encounter.dangerSigns /= EverySet.singleton NoDangerSign)
                    )
            )


countNewbornForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countNewbornForSelectedMonth selectedDate itemsList =
    itemsList
        |> List.filter
            (\item ->
                Maybe.map2
                    (\dateConcluded outcome ->
                        -- Live baby born within selected month.
                        (outcome == Pregnancy OutcomeLiveAtTerm || outcome == Pregnancy OutcomeLivePreTerm)
                            && withinSelectedMonth selectedDate dateConcluded
                    )
                    item.dateConcluded
                    item.outcome
                    |> Maybe.withDefault False
            )
        |> List.length


countPregnanciesDueWithin4MonthsForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countPregnanciesDueWithin4MonthsForSelectedMonth selectedDate itemsList =
    let
        dateFirstDayOfSelectedMonth =
            Date.floor Date.Month selectedDate
    in
    itemsList
        |> List.filter
            (\item ->
                let
                    -- Expected date exists, is within selected month or
                    -- latter than that, and within 120 days from the
                    -- beginning of selected month.
                    expectedDateConcludedFilter =
                        item.expectedDateConcluded
                            |> Maybe.map
                                (\expectedDateConcluded ->
                                    let
                                        compareResult =
                                            Date.compare expectedDateConcluded dateFirstDayOfSelectedMonth
                                    in
                                    (compareResult == GT || compareResult == EQ)
                                        && (Date.diff Date.Days dateFirstDayOfSelectedMonth expectedDateConcluded <= 120)
                                )
                            |> Maybe.withDefault False
                in
                isNothing item.dateConcluded && expectedDateConcludedFilter
            )
        |> List.length


countDeliveriesAtLocationForSelectedMonth : NominalDate -> DeliveryLocation -> List PrenatalDataItem -> Int
countDeliveriesAtLocationForSelectedMonth selectedDate location itemsList =
    itemsList
        |> List.filter
            (\item ->
                Maybe.map2
                    (\dateConcluded deliveryLocation ->
                        -- Live baby born within selected month.
                        withinSelectedMonth selectedDate dateConcluded
                            && (deliveryLocation == location)
                    )
                    item.dateConcluded
                    item.deliveryLocation
                    |> Maybe.withDefault False
            )
        |> List.length



--
-- Case management  functions.
--


getFollowUpsTotals : Language -> NominalDate -> ModelIndexedDb -> VillageId -> FollowUpMeasurements -> ( Int, Int, Int )
getFollowUpsTotals language currentDate db villageId followUps =
    let
        nutritionFollowUps =
            generateNutritionFollowUps db followUps
                |> filterVillageResidents villageId identity db

        nutritionEntries =
            generateNutritionFollowUpEntries language currentDate nutritionFollowUps db

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate acuteIllnessFollowUps db

        prenatalFollowUps =
            generatePrenatalFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        prenatalEntries =
            generatePrenatalFollowUpEntries language currentDate prenatalFollowUps db
    in
    ( List.length nutritionEntries
    , List.length acuteIllnessEntries
    , List.length prenatalEntries
    )


getAcuteIllnessFollowUpsBreakdownByDiagnosis : Language -> NominalDate -> ModelIndexedDb -> VillageId -> FollowUpMeasurements -> ( Int, Int, Int )
getAcuteIllnessFollowUpsBreakdownByDiagnosis language currentDate db villageId followUps =
    let
        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessEntries =
            generateAcuteIllnessFollowUpEntries language currentDate acuteIllnessFollowUps db

        covidEntries =
            List.filter (.diagnosis >> (==) DiagnosisCovid19) acuteIllnessEntries

        malariaEntries =
            List.filter
                (\entry ->
                    List.member entry.diagnosis
                        [ DiagnosisMalariaComplicated
                        , DiagnosisMalariaUncomplicated
                        , DiagnosisMalariaUncomplicatedAndPregnant
                        ]
                )
                acuteIllnessEntries

        giEntries =
            List.filter
                (\entry ->
                    List.member entry.diagnosis
                        [ DiagnosisGastrointestinalInfectionComplicated
                        , DiagnosisGastrointestinalInfectionUncomplicated
                        ]
                )
                acuteIllnessEntries
    in
    ( List.length covidEntries
    , List.length malariaEntries
    , List.length giEntries
    )



--
-- Helper functions.
--


withinSelectedMonth : NominalDate -> NominalDate -> Bool
withinSelectedMonth selectedDate date =
    let
        month =
            Date.monthNumber selectedDate

        year =
            Date.year selectedDate
    in
    (Date.monthNumber date == month)
        && (Date.year date == year)


getSelectedDate : NominalDate -> Model -> NominalDate
getSelectedDate currentDate model =
    Date.add Date.Months (-1 * model.monthGap) currentDate
