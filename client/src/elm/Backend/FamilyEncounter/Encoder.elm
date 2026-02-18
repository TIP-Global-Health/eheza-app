module Backend.FamilyEncounter.Encoder exposing (encodeFamilyEncounter)

import Backend.FamilyEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `FamilyEncounter`.
-}
encodeFamilyEncounter : FamilyEncounter -> List ( String, Value )
encodeFamilyEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "participants", list encodeEntityUuid encounter.participants )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "family_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
