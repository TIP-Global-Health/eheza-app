module Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements, mapPrenatalMeasurements)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, MotherMeasurementList, PrenatalMeasurements)
import Backend.Model exposing (..)
import RemoteData exposing (RemoteData(..))


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    let
        childMeasurements =
            Dict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | childMeasurements = Dict.insert childId childMeasurements model.childMeasurements }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    let
        motherMeasurements =
            Dict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | motherMeasurements = Dict.insert motherId motherMeasurements model.motherMeasurements }


mapPrenatalMeasurements : Maybe PrenatalEncounterId -> (PrenatalMeasurements -> PrenatalMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapPrenatalMeasurements id func model =
    case id of
        Just encounterId ->
            { model | prenatalMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.prenatalMeasurements }

        Nothing ->
            model
