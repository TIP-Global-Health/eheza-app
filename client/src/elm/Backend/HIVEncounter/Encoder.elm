module Backend.HIVEncounter.Encoder exposing (encodeHIVEncounter)

import Backend.HIVEncounter.Model exposing (HIVEncounter)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `HIVEncounter`.
-}
encodeHIVEncounter : HIVEncounter -> List ( String, Value )
encodeHIVEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "hiv_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
