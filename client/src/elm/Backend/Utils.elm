module Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements, mapNutritionMeasurements, mapPrenatalMeasurements)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, MotherMeasurementList, NutritionMeasurements, PrenatalMeasurements)
import Backend.Model exposing (..)
import RemoteData exposing (RemoteData(..))


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    let
        mapped =
            Dict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\measurements ->
                        Dict.insert childId (func measurements |> Success) model.childMeasurements
                    )
                |> Maybe.withDefault model.childMeasurements
    in
    { model | childMeasurements = mapped }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    let
        mapped =
            Dict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\measurements ->
                        Dict.insert motherId (func measurements |> Success) model.motherMeasurements
                    )
                |> Maybe.withDefault model.motherMeasurements
    in
    { model | motherMeasurements = mapped }


mapPrenatalMeasurements : Maybe PrenatalEncounterId -> (PrenatalMeasurements -> PrenatalMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapPrenatalMeasurements id func model =
    case id of
        Just encounterId ->
            { model | prenatalMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.prenatalMeasurements }

        Nothing ->
            model


mapNutritionMeasurements : Maybe NutritionEncounterId -> (NutritionMeasurements -> NutritionMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapNutritionMeasurements id func model =
    case id of
        Just encounterId ->
            { model | nutritionMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.nutritionMeasurements }

        Nothing ->
            model
