module Pages.HomeVisit.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getHomeVisitEncountersForParticipant, sortTuplesByDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.HomeVisit.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import ZScore.Model


generateAssembledData : HomeVisitEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.homeVisitEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.homeVisitMeasurements
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
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements id encounter_.participant db)
                |> Maybe.withDefault []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generatePreviousMeasurements : HomeVisitEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List ( NominalDate, ( HomeVisitEncounterId, HomeVisitMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    getHomeVisitEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter.
                if encounterId == currentEncounterId then
                    Nothing

                else
                    case Dict.get encounterId db.homeVisitMeasurements of
                        Just (Success data) ->
                            Just ( encounter.startDate, ( encounterId, data ) )

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortTuplesByDateDesc
