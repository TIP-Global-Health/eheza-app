module Backend.HealthCenter.Encoder exposing (encodeCatchmentArea, encodeHealthCenter)

import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeHealthCenter : HealthCenter -> List ( String, Value )
encodeHealthCenter healthCenter =
    [ ( "catchment_area", encodeEntityUuid healthCenter.catchmentAreaId )
    , ( "label", string healthCenter.name )

    -- @todo: Is there a problem with adding type?
    , ( "type", string "health_center" )
    ]


encodeCatchmentArea : CatchmentArea -> List ( String, Value )
encodeCatchmentArea catchmentArea =
    [ ( "label", string catchmentArea.name )
    , ( "type", string "catchment_area" )
    ]
