module Backend.HomeVisitEncounter.Encoder exposing (encodeHomeVisitEncounter)

import Backend.HomeVisitEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `HomeVisitEncounter`.
-}
encodeHomeVisitEncounter : HomeVisitEncounter -> List ( String, Value )
encodeHomeVisitEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "home_visit_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
