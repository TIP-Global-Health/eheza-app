module Backend.Village.Encoder exposing (encodeVillage)

import Backend.Village.Model exposing (Village)
import Json.Encode exposing (Value, bool, string)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeVillage : Village -> List ( String, Value )
encodeVillage village =
    [ ( "health_center", encodeEntityUuid village.healthCenterId )
    , ( "label", string village.name )
    , ( "province", string village.province )
    , ( "district", string village.district )
    , ( "sector", string village.sector )
    , ( "cell", string village.cell )
    , ( "village", string village.village )
    , ( "deleted", bool False )
    , ( "type", string "village" )
    ]
