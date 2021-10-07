module Pages.WellChildEncounter.Utils exposing (..)

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
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.WellChildActivity.Utils exposing (generateVaccinationProgress, getPreviousMeasurements)
import Pages.WellChildEncounter.Model exposing (..)
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

        immunisation =
            RemoteData.toMaybe measurements
                |> Maybe.andThen (.immunisation >> getMeasurementValueFunc)

        previousImmunisations =
            List.filterMap (.immunisation >> getMeasurementValueFunc)
                previousMeasurements

        vaccinationHistory =
            RemoteData.toMaybe measurements
                |> Maybe.andThen (.vaccinationHistory >> getMeasurementValueFunc)

        previousVaccinationHistories =
            List.filterMap (.vaccinationHistory >> getMeasurementValueFunc)
                previousMeasurements

        histories =
            Maybe.map (\history -> history :: previousVaccinationHistories) vaccinationHistory
                |> Maybe.withDefault previousVaccinationHistories

        vaccinationHistory_ =
            generateVaccinationProgress previousImmunisations histories

        vaccinationProgress =
            Maybe.map
                (\immunisation_ ->
                    generateVaccinationProgress (immunisation_ :: previousImmunisations) histories
                )
                immunisation
                |> Maybe.withDefault vaccinationHistory_
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)
        |> RemoteData.andMap (Success vaccinationHistory_)
        |> RemoteData.andMap (Success vaccinationProgress)


generatePreviousMeasurements : Maybe WellChildEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    getWellChildEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.wellChildMeasurements of
                        Just (Success data) ->
                            Just ( encounter.startDate, ( encounterId, data ) )

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortTuplesByDateDesc


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
