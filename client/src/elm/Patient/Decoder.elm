module Patient.Decoder
    exposing
        ( decodePatient
        , decodePatientsDict
        )

import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required)
import Patient.Model exposing (..)
import Utils.Json exposing (decodeListAsDict)


decodePatient : Decoder Patient
decodePatient =
    decode Patient
        |> required "label" string
        |> optionalAt [ "image", "styles", "large" ] string "http://placehold.it/350x150"


decodePatientsDict : Decoder PatientsDict
decodePatientsDict =
    decodeListAsDict decodePatient
