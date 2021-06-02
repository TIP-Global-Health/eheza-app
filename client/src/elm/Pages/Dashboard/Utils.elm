module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (DashboardStats, PrenatalDataItem)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (DangerSign(..))
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Pages.Dashboard.Model exposing (..)


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
                        withinSelectedMonth selectedDate encounter.created
                            && (not <| EverySet.isEmpty encounter.dangerSigns)
                            && (encounter.dangerSigns /= EverySet.singleton NoDangerSign)
                    )
            )


countTotalNewbornForSelectedMonth : NominalDate -> List PrenatalDataItem -> Int
countTotalNewbornForSelectedMonth selectedDate itemsList =
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
