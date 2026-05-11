module Pages.HomeVisit.Encounter.Utils exposing (generateAssembledData)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getHomeVisitEncountersForParticipant)
import Gizra.NominalDate exposing (NominalDate)
import Pages.HomeVisit.Encounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)


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
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) encounter_.participant db)
                |> Maybe.withDefault []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generatePreviousMeasurements :
    Maybe HomeVisitEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( HomeVisitEncounterId, HomeVisitMeasurements ) )
generatePreviousMeasurements =
    Backend.Measurement.Utils.generatePreviousMeasurements getHomeVisitEncountersForParticipant .homeVisitMeasurements
