module Backend.FamilyNutritionEncounter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model
import Backend.FamilyNutritionEncounter.Model exposing (FamilyNutritionEncounter)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( getMeasurementValueFunc
        , muacIndicationForChild
        , muacValueFunc
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Utils exposing (resolveFamilyParticipantForPerson)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


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
