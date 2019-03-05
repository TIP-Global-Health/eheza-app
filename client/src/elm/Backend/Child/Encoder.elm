module Backend.Child.Encoder exposing (encodeChild, encodeModeOfDelivery)

import Backend.Child.Model exposing (..)
import Backend.Patient.Encoder exposing (encodeGender)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeChild : Child -> List ( String, Value )
encodeChild child =
    [ ( "label", string child.name )
    , ( "avatar", maybe string child.avatarUrl )
    , ( "mother", maybe encodeEntityUuid child.motherId )
    , ( "date_birth", encodeYYYYMMDD child.birthDate )
    , ( "gender", encodeGender child.gender )
    ]


encodeModeOfDelivery : ModeOfDelivery -> String
encodeModeOfDelivery mode =
    case mode of
        VaginalDelivery vaginal ->
            case vaginal of
                Spontaneous True ->
                    "svd-episiotomy"

                Spontaneous False ->
                    "svd-no-episiotomy"

                WithVacuumExtraction ->
                    "vd-vacuum"

        CesareanDelivery ->
            "cesarean-delivery"
