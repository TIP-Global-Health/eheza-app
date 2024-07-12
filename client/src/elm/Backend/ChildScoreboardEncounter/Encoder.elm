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
encodeChildScoreboardEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "deleted", bool False )
    , ( "type", string "child_scoreboard_encounter" )
    ]
        ++ encodeIfSet "shard" session.shard encodeEntityUuid
