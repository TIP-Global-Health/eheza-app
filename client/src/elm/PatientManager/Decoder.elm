module PatientManager.Decoder
    exposing
        ( decodePatientFromResponse
        , decodePatientsFromResponse
        , decodeWeightFromResponse
        , decodeWeightListFromResponse
        )

import Child.Model exposing (..)
import Child.Decoder exposing (decodeWeight, decodeWeightList)
import Json.Decode exposing (at, Decoder)
import Patient.Model exposing (Patient, PatientsDict)
import Patient.Decoder exposing (decodePatient, decodePatientsDict)


decodePatientFromResponse : Decoder Patient
decodePatientFromResponse =
    at [ "data", "0" ] decodePatient


decodePatientsFromResponse : Decoder PatientsDict
decodePatientsFromResponse =
    at [ "data" ] decodePatientsDict


decodeWeightFromResponse : Decoder Weight
decodeWeightFromResponse =
    decodeWeight


decodeWeightListFromResponse : Decoder (List Weight)
decodeWeightListFromResponse =
    at [ "data" ] decodeWeightList
