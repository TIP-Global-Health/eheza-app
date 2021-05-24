module Pages.Dashboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (DashboardStats, PrenatalDataItem)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Date
import Gizra.NominalDate exposing (NominalDate)
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


getTotalPregnantForMonth : NominalDate -> NominalDate -> List PrenatalDataItem -> Int
getTotalPregnantForMonth currentDate selectedDate itemsList =
    let
        fromDate =
            Date.floor Date.Month selectedDate
    in
    itemsList
        |> List.filter
            (\item ->
                let
                    -- Expected date exists, and is set to 3 weeks or less, before
                    -- the beggining of the range.
                    expectedDateConcludedFilter =
                        item.expectedDateConcluded
                            |> Maybe.map
                                (\expectedDateConcluded ->
                                    Date.diff Date.Weeks expectedDateConcluded fromDate <= 3
                                )
                            |> Maybe.withDefault False

                    -- No date concluded, or it's set within month range, or after that.
                    actualDateConcludedFilter =
                        case item.dateConcluded of
                            Just dateConcluded ->
                                let
                                    compareResult =
                                        Date.compare fromDate dateConcluded
                                in
                                compareResult == LT || compareResult == EQ

                            Nothing ->
                                True
                in
                expectedDateConcludedFilter && actualDateConcludedFilter
            )
        |> List.length


getTotalNewbornForMonth : NominalDate -> List PrenatalDataItem -> Int
getTotalNewbornForMonth selectedDate itemsList =
    let
        month =
            Date.monthNumber selectedDate

        year =
            Date.year selectedDate
    in
    itemsList
        |> List.filter
            (\item ->
                Maybe.map2
                    (\dateConcluded outcome ->
                        -- Live baby born on same month and year.
                        (outcome == Pregnancy OutcomeLiveAtTerm || outcome == Pregnancy OutcomeLivePreTerm)
                            && (Date.monthNumber dateConcluded == month)
                            && (Date.year dateConcluded == year)
                    )
                    item.dateConcluded
                    item.outcome
                    |> Maybe.withDefault False
            )
        |> List.length


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
