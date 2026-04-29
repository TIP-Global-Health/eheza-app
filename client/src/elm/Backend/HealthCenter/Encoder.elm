module Backend.HealthCenter.Encoder exposing (encodeCatchmentArea, encodeHealthCenter)

import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Json.Encode exposing (Value, bool, string)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeHealthCenter : HealthCenter -> List ( String, Value )
encodeHealthCenter healthCenter =
    [ ( "catchment_area", encodeEntityUuid healthCenter.catchmentAreaId )
    , ( "label", string healthCenter.name )
    , ( "deleted", bool healthCenter.deleted )
    , ( "type", string "health_center" )
    ]


encodeCatchmentArea : CatchmentArea -> List ( String, Value )
encodeCatchmentArea catchmentArea =
    [ ( "label", string catchmentArea.name )
    , ( "deleted", bool False )
    , ( "type", string "catchment_area" )
    ]
