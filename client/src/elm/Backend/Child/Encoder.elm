module Backend.Child.Encoder exposing (encodeChild, encodeGender)

import Backend.Child.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeGender : Gender -> Value
encodeGender gender =
    case gender of
        Male ->
            string "male"

        Female ->
            string "female"


encodeChild : Child -> List ( String, Value )
encodeChild child =
    [ ( "label", string child.name )
    , ( "avatar", maybe string child.avatarUrl )
    , ( "mother", maybe encodeEntityUuid child.motherId )
    , ( "date_birth", encodeYYYYMMDD child.birthDate )
    , ( "gender", encodeGender child.gender )
    ]
