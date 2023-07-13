module Pages.WellChild.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant, sortTuplesByDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (..)
import Backend.WellChildEncounter.Model exposing (PediatricCareMilestone(..), WellChildEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.WellChild.Activity.Utils exposing (generateVaccinationProgress, getPreviousMeasurements)
import Pages.WellChild.Encounter.Model exposing (..)
import Pages.WellChild.Utils exposing (generatePreviousMeasurements)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : WellChildEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.wellChildEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.wellChildMeasurements
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
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) encounter_.participant db)
                |> Maybe.withDefault []

        previousMeasurements =
            getPreviousMeasurements previousMeasurementsWithDates

        ( vaccinationHistory, vaccinationProgress ) =
            RemoteData.toMaybe person
                |> Maybe.map
                    (\person_ ->
                        ( generateVaccinationProgress person_ previousMeasurements
                        , RemoteData.toMaybe measurements
                            |> Maybe.map (\measurements_ -> measurements_ :: previousMeasurements)
                            |> Maybe.withDefault previousMeasurements
                            |> generateVaccinationProgress person_
                        )
                    )
                |> Maybe.withDefault ( Dict.empty, Dict.empty )
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)
        |> RemoteData.andMap (Success vaccinationHistory)
        |> RemoteData.andMap (Success vaccinationProgress)


resolvePediatricCareMilestoneOnDate : NominalDate -> NominalDate -> Maybe PediatricCareMilestone
resolvePediatricCareMilestoneOnDate dueDate birthDate =
    let
        ageWeeks =
            Date.diff Weeks birthDate dueDate

        ageMonths =
            Date.diff Months birthDate dueDate
    in
    if ageWeeks < 6 then
        Nothing

    else if ageWeeks < 14 then
        Just Milestone6Weeks

    else if ageMonths < 6 then
        Just Milestone14Weeks

    else if ageMonths < 9 then
        Just Milestone6Months

    else if ageMonths < 12 then
        Just Milestone9Months

    else if ageMonths < 15 then
        Just Milestone12Months

    else if ageMonths < 18 then
        Just Milestone15Months

    else if ageMonths < 24 then
        Just Milestone18Months

    else if ageMonths < 36 then
        Just Milestone2Years

    else if ageMonths < 48 then
        Just Milestone3Years

    else
        Just Milestone4Years


resolveDateForPediatricCareMilestone : NominalDate -> PediatricCareMilestone -> NominalDate
resolveDateForPediatricCareMilestone birthDate milestone =
    case milestone of
        Milestone6Weeks ->
            Date.add Weeks 6 birthDate

        Milestone14Weeks ->
            Date.add Weeks 14 birthDate

        Milestone6Months ->
            Date.add Months 6 birthDate

        Milestone9Months ->
            Date.add Months 9 birthDate

        Milestone12Months ->
            Date.add Years 1 birthDate

        Milestone15Months ->
            Date.add Months 15 birthDate

        Milestone18Months ->
            Date.add Months 18 birthDate

        Milestone2Years ->
            Date.add Years 2 birthDate

        Milestone3Years ->
            Date.add Years 3 birthDate

        Milestone4Years ->
            Date.add Years 4 birthDate


pediatricCareMilestoneToComparable : PediatricCareMilestone -> Int
pediatricCareMilestoneToComparable milestone =
    case milestone of
        Milestone6Weeks ->
            1

        Milestone14Weeks ->
            2

        Milestone6Months ->
            3

        Milestone9Months ->
            4

        Milestone12Months ->
            5

        Milestone15Months ->
            6

        Milestone18Months ->
            7

        Milestone2Years ->
            8

        Milestone3Years ->
            9

        Milestone4Years ->
            10
