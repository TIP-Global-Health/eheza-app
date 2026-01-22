module Backend.ChildScoreboardEncounter.Encoder exposing (encodeChildScoreboardEncounter)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `ChildScoreboardEncounter`.
-}
encodeChildScoreboardEncounter : ChildScoreboardEncounter -> List ( String, Value )
encodeChildScoreboardEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "child_scoreboard_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
