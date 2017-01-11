module Patient.Utils
    exposing
        ( getPatientName
        )

import Patient.Model exposing (Patient, PatientType(..))


getPatientName : Patient -> String
getPatientName patient =
    case patient.info of
        PatientChild child ->
            .name child

        PatientMother mother ->
            .name mother
