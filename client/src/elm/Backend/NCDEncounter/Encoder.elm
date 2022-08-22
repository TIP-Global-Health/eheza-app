module Backend.NCDEncounter.Encoder exposing (encodeNCDEncounter)

import Backend.NCDEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


encodeNCDEncounter : NCDEncounter -> List ( String, Value )
encodeNCDEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool False )
    , ( "type", string "ncd_encounter" )
    ]
        ++ encodeIfExists "shard" encounter.shard encodeEntityUuid
