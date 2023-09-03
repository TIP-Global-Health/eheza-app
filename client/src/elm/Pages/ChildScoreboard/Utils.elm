module Pages.ChildScoreboard.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getChildScoreboardEncountersForParticipant)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils exposing (getPreviousMeasurements, mergeVaccinationProgressDicts)
import Pages.ChildScoreboard.Activity.Utils
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.WellChild.Activity.Utils
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


generatePreviousMeasurements :
    Maybe ChildScoreboardEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( ChildScoreboardEncounterId, ChildScoreboardMeasurements ) )
generatePreviousMeasurements =
    Backend.Measurement.Utils.generatePreviousMeasurements getChildScoreboardEncountersForParticipant .childScoreboardMeasurements


generateVaccinationProgressDicts : AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDicts assembled db =
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
                        |> Pages.WellChild.Activity.Utils.generateVaccinationProgress assembled.person
                )
                individualWellChildParticipantId
                |> Maybe.withDefault Dict.empty

        vaccinationHistory =
            Pages.ChildScoreboard.Activity.Utils.generateVaccinationProgress previousMeasurements

        vaccinationProgress =
            assembled.measurements
                :: previousMeasurements
                |> Pages.ChildScoreboard.Activity.Utils.generateVaccinationProgress
    in
    ( mergeVaccinationProgressDicts
        vaccinationHistory
        vaccinationProgressByWellChild
    , mergeVaccinationProgressDicts
        vaccinationProgress
        vaccinationProgressByWellChild
    )
