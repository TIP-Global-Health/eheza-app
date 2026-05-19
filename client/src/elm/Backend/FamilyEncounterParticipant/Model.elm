module Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant, FamilyEncounterType(..), FamilyParticipantInitiator(..), emptyFamilyEncounterParticipant)

import Backend.Entities exposing (..)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Gizra.NominalDate exposing (NominalDate)


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
