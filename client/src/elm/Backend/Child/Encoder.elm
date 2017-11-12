module Backend.Child.Encoder exposing (..)

import Backend.Child.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityId)


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
    , ( "avatar", string child.image )
    , ( "mother", maybe encodeEntityId child.motherId )
    , ( "sibling", maybe encodeEntityId child.siblingId )
    , ( "date_birth", encodeYYYYMMDD child.birthDate )
    , ( "gender", encodeGender child.gender )
    ]
