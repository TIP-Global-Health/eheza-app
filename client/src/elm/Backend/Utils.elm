module Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, MotherMeasurementList)
import Backend.Model exposing (..)
import EveryDict
import RemoteData exposing (RemoteData(..))


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    let
        childMeasurements =
            EveryDict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | childMeasurements = EveryDict.insert childId childMeasurements model.childMeasurements }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    let
        motherMeasurements =
            EveryDict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | motherMeasurements = EveryDict.insert motherId motherMeasurements model.motherMeasurements }
