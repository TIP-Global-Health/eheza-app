module Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements)

import AllDict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, MotherMeasurementList)
import Backend.Model exposing (..)
import RemoteData exposing (RemoteData(..))


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    let
        childMeasurements =
            AllDict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | childMeasurements = AllDict.insert childId childMeasurements model.childMeasurements }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    let
        motherMeasurements =
            AllDict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.map func
    in
    { model | motherMeasurements = AllDict.insert motherId motherMeasurements model.motherMeasurements }
