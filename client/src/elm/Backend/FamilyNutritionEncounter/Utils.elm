module Backend.FamilyNutritionEncounter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model
import Backend.FamilyNutritionEncounter.Model exposing (FamilyNutritionEncounter)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( getMeasurementValueFunc
        , muacIndication
        , muacValueFunc
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Utils exposing (resolveFamilyParticipantForPerson)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


generateFamilyFamilyNutritionMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( FamilyNutritionEncounterId, FamilyNutritionMeasurements ) )
generateFamilyFamilyNutritionMeasurementsForChild childId db =
    resolveFamilyParticipantForPerson childId Backend.FamilyEncounterParticipant.Model.FamilyNutritionEncounter db
        |> Maybe.map
            (getFamilyNutritionEncountersForParticipant db
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        case Dict.get encounterId db.nutritionMeasurements of
                            Just (Success data) ->
                                Just ( encounter.startDate, ( encounterId, data ) )

                            _ ->
                                Nothing
                    )
                -- Most recent date to least recent date.
                >> List.sortWith sortTuplesByDateDesc
            )
        |> Maybe.withDefault []


getFamilyNutritionEncountersForParticipant : ModelIndexedDb -> FamilyEncounterParticipantId -> List ( FamilyNutritionEncounterId, FamilyNutritionEncounter )
getFamilyNutritionEncountersForParticipant =
    getParticipantEncountersByEncounterType .familyNutritionEncountersByParticipant


getParticipantEncountersByEncounterType :
    (ModelIndexedDb -> Dict FamilyEncounterParticipantId (WebData (Dict encounterId encounter)))
    -> ModelIndexedDb
    -> FamilyEncounterParticipantId
    -> List ( encounterId, encounter )
getParticipantEncountersByEncounterType dataStructureFunc db participantId =
    dataStructureFunc db
        |> Dict.get participantId
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
