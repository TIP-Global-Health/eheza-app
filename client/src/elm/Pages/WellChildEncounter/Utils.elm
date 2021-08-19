module Pages.WellChildEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant, sortTuplesByDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (..)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
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


resolveEncounterTypeOnDate : Bool -> NominalDate -> NominalDate -> WellChildEncounterType
resolveEncounterTypeOnDate isChw duewDate birthDate =
    let
        ageWeeks =
            Date.diff Weeks birthDate duewDate

        ageMonths =
            Date.diff Months birthDate duewDate
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


encounterToComparable : WellChildEncounterType -> Int
encounterToComparable encounterType =
    case encounterType of
        NewbornExam ->
            0

        PediatricCareBirthTo6Weeks ->
            1

        PediatricCare6Weeks ->
            2

        PediatricCare10Weeks ->
            3

        PediatricCare14Weeks ->
            4

        PediatricCare6Months ->
            5

        PediatricCare9Months ->
            6

        PediatricCare12Months ->
            7

        PediatricCare15Months ->
            8

        PediatricCare18Months ->
            9

        PediatricCareRecurrent ->
            10


dueDateForEncounter : NominalDate -> WellChildEncounterType -> NominalDate
dueDateForEncounter birthDate encounterType =
    case encounterType of
        NewbornExam ->
            birthDate

        PediatricCareBirthTo6Weeks ->
            birthDate

        PediatricCare6Weeks ->
            Date.add Weeks 6 birthDate

        PediatricCare10Weeks ->
            Date.add Weeks 10 birthDate

        PediatricCare14Weeks ->
            Date.add Weeks 14 birthDate

        PediatricCare6Months ->
            Date.add Months 6 birthDate

        PediatricCare9Months ->
            Date.add Months 9 birthDate

        PediatricCare12Months ->
            Date.add Months 12 birthDate

        PediatricCare15Months ->
            Date.add Months 15 birthDate

        PediatricCare18Months ->
            Date.add Months 18 birthDate

        PediatricCareRecurrent ->
            Date.add Months 24 birthDate


getNextPediatricCareEncounter : WellChildEncounterType -> WellChildEncounterType
getNextPediatricCareEncounter encounterType =
    List.Extra.elemIndex encounterType allPediatricCareEncounters
        |> Maybe.andThen (\position -> List.Extra.getAt (position + 1) allPediatricCareEncounters)
        |> Maybe.withDefault PediatricCareRecurrent


allPediatricCareEncounters : List WellChildEncounterType
allPediatricCareEncounters =
    [ PediatricCareBirthTo6Weeks
    , PediatricCare6Weeks
    , PediatricCare10Weeks
    , PediatricCare14Weeks
    , PediatricCare6Months
    , PediatricCare9Months
    , PediatricCare12Months
    , PediatricCare15Months
    , PediatricCare18Months
    , PediatricCareRecurrent
    ]
