module Backend.NutritionEncounter.Encoder exposing (encodeNutritionEncounter)

import Backend.NutritionEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


{-| Encodes a `NutritionEncounter`.
-}
encodeNutritionEncounter : NutritionEncounter -> List ( String, Value )
encodeNutritionEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_encounter" )
    ]
        ++ encodeIfExists "shard" session.shard encodeEntityUuid
