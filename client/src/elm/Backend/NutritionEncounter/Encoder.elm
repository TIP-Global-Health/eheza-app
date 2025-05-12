module Backend.NutritionEncounter.Encoder exposing (encodeNutritionEncounter, encodeNutritionEncounterType)

import Backend.Measurement.Encoder exposing (encodeSkippedForm)
import Backend.NutritionEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeEverySet, encodeIfSet)


{-| Encodes a `NutritionEncounter`.
-}
encodeNutritionEncounter : NutritionEncounter -> List ( String, Value )
encodeNutritionEncounter encounter =
    let
        skippedForms =
            if EverySet.isEmpty encounter.skippedForms then
                []

            else
                [ ( "skipped_forms", encodeEverySet encodeSkippedForm encounter.skippedForms ) ]
    in
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "nutrition_encounter_type", encodeNutritionEncounterType encounter.encounterType )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_encounter" )
    ]
        ++ skippedForms
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid


encodeNutritionEncounterType : NutritionEncounterType -> Value
encodeNutritionEncounterType encounterType =
    string <|
        case encounterType of
            NutritionEncounterNurse ->
                "nurse"

            NutritionEncounterCHW ->
                "chw"

            NutritionEncounterUnknown ->
                "unknown"
