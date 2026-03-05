module Backend.FamilyEncounterParticipant.Model exposing (..)

import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessEncounterType)
import Backend.Entities exposing (..)
import Backend.NutritionEncounter.Model exposing (NutritionEncounterType)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias FamilyEncounterParticipant =
    { person : PersonId
    , encounterType : FamilyEncounterType
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyFamilyEncounterParticipant : NominalDate -> PersonId -> FamilyEncounterType -> HealthCenterId -> FamilyEncounterParticipant
emptyFamilyEncounterParticipant currentDate personId type_ healthCenterId =
    { person = personId
    , encounterType = type_
    , startDate = currentDate
    , endDate = Nothing
    , deleted = False
    , shard = Just healthCenterId
    }


type FamilyParticipantInitiator
    = InitiatorParticipantsPage
    | InitiatorPatientRecord PatientRecordInitiator PersonId


type FamilyEncounterType
    = NutritionEncounter
