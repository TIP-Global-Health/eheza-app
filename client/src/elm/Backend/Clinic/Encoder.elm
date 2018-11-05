module Backend.Clinic.Encoder exposing (encodeClinic)

import Backend.Clinic.Model exposing (..)
import Json.Encode exposing (..)


encodeClinic : Clinic -> List ( String, Value )
encodeClinic clinic =
    [ ( "label", string clinic.name )
    ]
