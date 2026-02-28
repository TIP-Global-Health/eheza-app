module Backend.FamilyNutritionEncounter.Encoder exposing (encodeFamilyNutritionEncounter)

import Backend.FamilyNutritionEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `FamilyNutritionEncounter`.
-}
encodeFamilyNutritionEncounter : FamilyNutritionEncounter -> List ( String, Value )
encodeFamilyNutritionEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "family_participant", encodeEntityUuid encounter.participant )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "family_nutrition_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid
