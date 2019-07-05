module Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements, mapPrenatalMeasurements)

import AllDict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, MotherMeasurementList, PrenatalMeasurements)
import Backend.Model exposing (..)
import RemoteData exposing (RemoteData(..))


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    { model | childMeasurements = AllDict.update childId (Maybe.map (RemoteData.map func)) model.childMeasurements }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    { model | motherMeasurements = AllDict.update motherId (Maybe.map (RemoteData.map func)) model.motherMeasurements }


mapPrenatalMeasurements : Maybe PrenatalEncounterId -> (PrenatalMeasurements -> PrenatalMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapPrenatalMeasurements id func model =
    case id of
        Just encounterId ->
            { model | prenatalMeasurements = AllDict.update encounterId (Maybe.map (RemoteData.map func)) model.prenatalMeasurements }

        Nothing ->
            model
