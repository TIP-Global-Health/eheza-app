module Backend.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)


type PatientRecordInitiator
    = InitiatorParticipantDirectory
    | InitiatorPatientRecord PersonId
