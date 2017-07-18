module Patient.Utils
    exposing
        ( getPatientAvatarThumb
        , getPatientName
        , getPatientType
        )

import Patient.Model exposing (Patient, PatientType(..))


getPatientAvatarThumb : Patient -> String
getPatientAvatarThumb patient =
    case patient.info of
        PatientChild child ->
            .image child

        PatientMother mother ->
            .image mother


getPatientName : Patient -> String
getPatientName patient =
    case patient.info of
        PatientChild child ->
            .name child

        PatientMother mother ->
            .name mother


getPatientType : Patient -> String
getPatientType patient =
    case patient.info of
        PatientChild child ->
            "child"

        PatientMother mother ->
            "mother"
