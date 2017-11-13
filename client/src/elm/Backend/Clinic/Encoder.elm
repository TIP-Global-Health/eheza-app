module Backend.Clinic.Encoder exposing (..)

import Backend.Clinic.Model exposing (..)
import Json.Encode exposing (..)


encodeClinic : Clinic -> Value
encodeClinic clinic =
    object
        [ ( "label", string clinic.name )
        ]
