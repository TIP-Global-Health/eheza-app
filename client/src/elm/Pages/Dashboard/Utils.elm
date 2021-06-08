module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model exposing (AcuteIllnessDataItem, AcuteIllnessEncounterDataItem, DashboardStats, PrenatalDataItem)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (Call114Sign(..), DangerSign(..), FollowUpMeasurements, IsolationSign(..), SendToHCSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (filterVillageResidents, generateAcuteIllnessFollowUps, generateNutritionFollowUps, generatePrenatalFollowUps)
import Pages.GlobalCaseManagement.View exposing (viewAcuteIllnessFollowUpEntries, viewNutritionFollowUpEntries, viewPrenatalFollowUpEntries)
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


getAcuteIllnessAssesmentsForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem
getAcuteIllnessAssesmentsForSelectedMonth selectedDate itemsList =
    List.map .encounters itemsList
        |> List.concat
        |> List.filter (.startDate >> withinSelectedMonth selectedDate)


countAcuteIllnessAssesments : List AcuteIllnessEncounterDataItem -> Int
countAcuteIllnessAssesments encounters =
    -- Count number of encounters that occured during selected month.
    List.length encounters


countAcuteIllnessCasesByHCReferrals : List AcuteIllnessEncounterDataItem -> ( Int, Int )
countAcuteIllnessCasesByHCReferrals encounters =
    let
        ( sentToHC, managedLocally ) =
            List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) encounters
                |> List.partition (\encounter -> EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
    in
    ( List.length sentToHC, List.length managedLocally )


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


countDiagnosedWithCovidCallsTo114ForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countDiagnosedWithCovidCallsTo114ForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected
                    -- month which has produced Covid19 diagnosis,
                    -- and there was a call to 114.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisCovid19)
                        && EverySet.member Call114 encounter.call114Signs
                )
        )
        itemsList
        |> List.length


countCovidSentToHCForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countCovidSentToHCForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected
                    -- month which has produced Covid19 diagnosis,
                    -- and patient was sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisCovid19)
                        && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
                )
        )
        itemsList
        |> List.length


countCovidManagedAtHomeForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countCovidManagedAtHomeForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected
                    -- month which has produced Covid19 diagnosis,
                    -- and patient was isolated at home.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisCovid19)
                        && EverySet.member PatientIsolated encounter.isolationSigns
                )
        )
        itemsList
        |> List.length



--
-- Acute illness - Malaria functions.
--


countDiagnosedWithMalariaForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countDiagnosedWithMalariaForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced a Malaria diagnosis.
                    withinSelectedMonth selectedDate encounter.startDate
                        && List.member encounter.diagnosis
                            [ DiagnosisMalariaComplicated
                            , DiagnosisMalariaUncomplicated
                            , DiagnosisMalariaUncomplicatedAndPregnant
                            ]
                )
        )
        itemsList
        |> List.length


countUncomplicatedMalariaManagedByChwForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countUncomplicatedMalariaManagedByChwForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced Uncomlicated Malaria diagnosis,
                    -- and patient was not sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisMalariaUncomplicated)
                        && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
                )
        )
        itemsList
        |> List.length


countUncomplicatedMalariaAndPregnantSentToHCForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countUncomplicatedMalariaAndPregnantSentToHCForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced Uncomlicated Malaria and Pregnant diagnosis,
                    -- and patient was sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisMalariaUncomplicatedAndPregnant)
                        && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
                )
        )
        itemsList
        |> List.length


countComplicatedMalariaSentToHCForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countComplicatedMalariaSentToHCForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced Comlicated Malaria diagnosis,
                    -- and patient was sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisMalariaComplicated)
                        && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
                )
        )
        itemsList
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


countDiagnosedWithGIForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countDiagnosedWithGIForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced a Gastro Infection diagnosis.
                    withinSelectedMonth selectedDate encounter.startDate
                        && List.member encounter.diagnosis
                            [ DiagnosisGastrointestinalInfectionComplicated
                            , DiagnosisGastrointestinalInfectionUncomplicated
                            ]
                )
        )
        itemsList
        |> List.length


countUncomplicatedGIManagedByChwForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countUncomplicatedGIManagedByChwForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced Uncomlicated GI diagnosis,
                    -- and patient was not sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated)
                        && not (EverySet.member ReferToHealthCenter encounter.sendToHCSigns)
                )
        )
        itemsList
        |> List.length


countComplicatedGISentToHCForSelectedMonth : NominalDate -> List AcuteIllnessDataItem -> Int
countComplicatedGISentToHCForSelectedMonth selectedDate itemsList =
    List.filter
        (.encounters
            >> List.any
                (\encounter ->
                    -- Illness that got an encounter at selected month
                    -- which has produced Comlicated GI diagnosis,
                    -- and patient was sent to health center.
                    withinSelectedMonth selectedDate encounter.startDate
                        && (encounter.diagnosis == DiagnosisGastrointestinalInfectionComplicated)
                        && EverySet.member ReferToHealthCenter encounter.sendToHCSigns
                )
        )
        itemsList
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
    in
    itemsList
        |> List.filter
            (\item ->
                let
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
                expectedDateConcludedFilter && actualDateConcludedFilter
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
            viewNutritionFollowUpEntries language currentDate nutritionFollowUps db

        acuteIllnessFollowUps =
            generateAcuteIllnessFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        acuteIllnessEntries =
            viewAcuteIllnessFollowUpEntries language currentDate acuteIllnessFollowUps db

        prenatalFollowUps =
            generatePrenatalFollowUps db followUps
                |> filterVillageResidents villageId Tuple.second db

        prenatalEntries =
            viewPrenatalFollowUpEntries language currentDate prenatalFollowUps db
    in
    ( List.length nutritionEntries
    , List.length acuteIllnessEntries
    , List.length prenatalEntries
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
