module Backend.Child.Encoder exposing (encodeChild)

import Backend.Child.Model exposing (..)
import Backend.Patient.Encoder exposing (encodeGender)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityId)


encodeChild : Child -> List ( String, Value )
encodeChild child =
    [ ( "label", string child.name )
    , ( "avatar", maybe string child.avatarUrl )
    , ( "mother", maybe encodeEntityId child.motherId )
    , ( "date_birth", encodeYYYYMMDD child.birthDate )
    , ( "gender", encodeGender child.gender )
    ]
