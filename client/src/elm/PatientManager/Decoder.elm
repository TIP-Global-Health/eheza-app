module PatientManager.Decoder
    exposing
        ( decodePatientFromResponse
        , decodePatientsFromResponse
        )

import Json.Decode exposing (at, Decoder)
import Patient.Model exposing (Patient, PatientsDict)
import Patient.Decoder exposing (decodePatient, decodePatientsDict)


decodePatientFromResponse : Decoder Patient
decodePatientFromResponse =
    at [ "data", "0" ] decodePatient


decodePatientsFromResponse : Decoder PatientsDict
decodePatientsFromResponse =
    at [ "data" ] decodePatientsDict
