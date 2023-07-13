module Pages.WellChild.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementDateMeasuredFunc, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils exposing (getPreviousMeasurements, mergeVaccinationProgressDicts)
import Pages.ChildScoreboard.Activity.Utils
import Pages.ChildScoreboard.Utils
import Pages.WellChild.Activity.Utils
import Pages.WellChild.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


generatePreviousMeasurements :
    Maybe WellChildEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
generatePreviousMeasurements =
    Backend.Measurement.Utils.generatePreviousMeasurements getWellChildEncountersForParticipant .wellChildMeasurements


generateVaccinationProgressDicts : AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDicts assembled db =
    let
        previousMeasurements =
            getPreviousMeasurements assembled.previousMeasurementsWithDates

        vaccinationProgressByChildScoreboard =
            let
                individualParticipants =
                    Dict.get assembled.participant.person db.individualParticipantsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map Dict.toList
                        |> Maybe.withDefault []

                individualChildScoreboardParticipantId =
                    List.filter
                        (Tuple.second
                            >> .encounterType
                            >> (==) Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        )
                        individualParticipants
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            Maybe.map
                (\participantId ->
                    Pages.ChildScoreboard.Utils.generatePreviousMeasurements Nothing participantId db
                        |> getPreviousMeasurements
                        |> Pages.ChildScoreboard.Activity.Utils.generateVaccinationProgress
                )
                individualChildScoreboardParticipantId
                |> Maybe.withDefault Dict.empty

        vaccinationHistory =
            Pages.WellChild.Activity.Utils.generateVaccinationProgress assembled.person previousMeasurements

        vaccinationProgress =
            assembled.measurements
                :: previousMeasurements
                |> Pages.WellChild.Activity.Utils.generateVaccinationProgress assembled.person
    in
    ( mergeVaccinationProgressDicts
        vaccinationHistory
        vaccinationProgressByChildScoreboard
    , mergeVaccinationProgressDicts
        vaccinationProgress
        vaccinationProgressByChildScoreboard
    )
