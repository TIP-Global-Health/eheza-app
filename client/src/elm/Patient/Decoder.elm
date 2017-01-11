module Patient.Decoder
    exposing
        ( decodePatient
        , decodePatientsDict
        )

import Child.Decoder exposing (decodeChild)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Mother.Decoder exposing (decodeMother)
import Patient.Model exposing (..)
import Utils.Json exposing (decodeListAsDict)


decodePatient : Decoder Patient
decodePatient =
    decode Patient
        |> custom decodePatientType
        |> hardcoded ""


decodePatientType : Decoder PatientType
decodePatientType =
    field "type" string
        |> andThen
            (\type_ ->
                case type_ of
                    "child" ->
                        map PatientChild decodeChild

                    "mother" ->
                        map PatientMother decodeMother

                    _ ->
                        fail (type_ ++ " is not a recognized 'type' for PatientType.")
            )


decodePatientsDict : Decoder PatientsDict
decodePatientsDict =
    decodeListAsDict decodePatient
