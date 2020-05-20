module Backend.HealthCenter.Encoder exposing (encodeHealthCenter)

import Backend.HealthCenter.Model exposing (HealthCenter)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeHealthCenter : HealthCenter -> Value
encodeHealthCenter healthCenter =
    object
        [ ( "catchment_area", encodeEntityUuid healthCenter.catchmentAreaId )
        , ( "name", string healthCenter.name )
        ]
