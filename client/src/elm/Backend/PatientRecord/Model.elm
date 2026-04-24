module Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))

import Backend.Entities exposing (..)


type PatientRecordInitiator
    = InitiatorParticipantDirectory
    | InitiatorPatientRecord PersonId
