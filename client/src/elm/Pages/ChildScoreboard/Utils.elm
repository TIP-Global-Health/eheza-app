module Pages.ChildScoreboard.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getChildScoreboardEncountersForParticipant)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils exposing (getPreviousMeasurements, mergeVaccinationProgressDicts)
import Pages.ChildScoreboard.Activity.Utils
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.WellChild.Activity.Utils
import RemoteData
import SyncManager.Model exposing (Site)


generatePreviousMeasurements :
    Maybe ChildScoreboardEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( ChildScoreboardEncounterId, ChildScoreboardMeasurements ) )
generatePreviousMeasurements =
    Backend.Measurement.Utils.generatePreviousMeasurements getChildScoreboardEncountersForParticipant .childScoreboardMeasurements


generateVaccinationProgressDicts : Site -> AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDicts site assembled db =
    let
        previousMeasurements =
            getPreviousMeasurements assembled.previousMeasurementsWithDates

        vaccinationProgressByWellChild =
            let
                individualParticipants =
                    Dict.get assembled.participant.person db.individualParticipantsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map Dict.toList
                        |> Maybe.withDefault []

                individualWellChildParticipantId =
                    List.filter
                        (Tuple.second
                            >> .encounterType
                            >> (==) Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        )
                        individualParticipants
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            Maybe.map
                (\participantId ->
                    Backend.Measurement.Utils.generatePreviousMeasurements
                        Backend.NutritionEncounter.Utils.getWellChildEncountersForParticipant
                        .wellChildMeasurements
                        Nothing
                        participantId
                        db
                        |> getPreviousMeasurements
                        |> Pages.WellChild.Activity.Utils.generateVaccinationProgress site assembled.person
                )
                individualWellChildParticipantId
                |> Maybe.withDefault Dict.empty

        vaccinationHistory =
            Pages.ChildScoreboard.Activity.Utils.generateVaccinationProgress site previousMeasurements

        vaccinationProgress =
            assembled.measurements
                :: previousMeasurements
                |> Pages.ChildScoreboard.Activity.Utils.generateVaccinationProgress site
    in
    ( mergeVaccinationProgressDicts
        vaccinationHistory
        vaccinationProgressByWellChild
    , mergeVaccinationProgressDicts
        vaccinationProgress
        vaccinationProgressByWellChild
    )
