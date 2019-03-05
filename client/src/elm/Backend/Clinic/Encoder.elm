module Backend.Clinic.Encoder exposing (encodeClinic)

import Backend.Clinic.Model exposing (..)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeClinic : Clinic -> List ( String, Value )
encodeClinic clinic =
    [ ( "label", string clinic.name )
    , ( "health_center", encodeEntityUuid clinic.healthCenterId )
    ]
