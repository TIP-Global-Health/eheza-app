module Backend.Utils exposing (mapAcuteIllnessMeasurements, mapChildMeasurements, mapMotherMeasurements, mapNutritionMeasurements, mapPrenatalMeasurements, saveMeasurementCmd, sw)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , ChildMeasurementList
        , HomeVisitMeasurements
        , MotherMeasurementList
        , NutritionMeasurements
        , PrenatalMeasurements
        )
import Backend.Model exposing (..)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, toEntityUuid, withoutDecoder)


sw : Restful.Endpoint.CrudOperations w e k v c p
sw =
    applyBackendUrl "/sw"


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


mapAcuteIllnessMeasurements : Maybe AcuteIllnessEncounterId -> (AcuteIllnessMeasurements -> AcuteIllnessMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapAcuteIllnessMeasurements id func model =
    case id of
        Just encounterId ->
            { model | acuteIllnessMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.acuteIllnessMeasurements }

        Nothing ->
            model


mapHomeVisitMeasurements : Maybe HomeVisitEncounterId -> (HomeVisitMeasurements -> HomeVisitMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapHomeVisitMeasurements id func model =
    case id of
        Just encounterId ->
            { model | homeVisitMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.homeVisitMeasurements }

        Nothing ->
            model


saveMeasurementCmd date encounter person nurse healthCenter savedValueId savedValue endpoint handleSavedMsg =
    let
        measurement =
            { participantId = person
            , dateMeasured = date
            , encounterId = Just encounter
            , nurse = nurse
            , healthCenter = healthCenter
            , value = savedValue
            }

        requestData =
            case savedValueId of
                Nothing ->
                    measurement
                        |> sw.post endpoint
                        |> withoutDecoder

                Just id ->
                    measurement
                        |> sw.patchFull endpoint id
                        |> withoutDecoder
    in
    toCmd (RemoteData.fromResult >> handleSavedMsg) requestData
