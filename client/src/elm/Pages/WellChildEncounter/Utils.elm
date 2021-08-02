module Pages.WellChildEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (..)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime)
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
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> RemoteData.withDefault []

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


generatePreviousMeasurements : WellChildEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) ))
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.wellChildEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (Dict.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case Dict.get encounterId db.wellChildMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, ( encounterId, data ) )

                                _ ->
                                    Nothing
                    )
                -- Most recent date to least recent date.
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> Date.compare date2 date1)
            )


encounterTypeByAge : NominalDate -> Bool -> NominalDate -> WellChildEncounterType
encounterTypeByAge currentDate isChw birthDate =
    let
        ageWeeks =
            Date.diff Weeks birthDate currentDate

        ageMonths =
            Date.diff Months birthDate currentDate
    in
    if isChw then
        NewbornExam

    else if ageWeeks < 6 then
        PediatricCareBirthTo6Weeks

    else if ageWeeks < 10 then
        PediatricCare6Weeks

    else if ageWeeks < 14 then
        PediatricCare10Weeks

    else if ageMonths < 6 then
        PediatricCare14Weeks

    else if ageMonths < 9 then
        PediatricCare6Months

    else if ageMonths < 12 then
        PediatricCare9Months

    else if ageMonths < 15 then
        PediatricCare12Months

    else if ageMonths < 18 then
        PediatricCare15Months

    else if ageMonths < 24 then
        PediatricCare18Months

    else
        PediatricCareRecurrent
