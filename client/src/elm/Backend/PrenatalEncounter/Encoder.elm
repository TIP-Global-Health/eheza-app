module Backend.PrenatalEncounter.Encoder exposing (encodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


{-| Encodes a `PrenatalEncounter`.
-}
encodePrenatalEncounter : PrenatalEncounter -> List ( String, Value )
encodePrenatalEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "prenatal_encounter_type", encodePrenatalEncounterType session.encounterType )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_encounter" )
    ]
        ++ encodeIfExists "shard" session.shard encodeEntityUuid


encodePrenatalEncounterType : PrenatalEncounterType -> Value
encodePrenatalEncounterType encounterType =
    string <|
        case encounterType of
            NurseEncounter ->
                "nurse"

            ChwFirstEncounter ->
                "chw-1"

            ChwSecondEncounter ->
                "chw-2"

            ChwThirdPlusEncounter ->
                "chw-3"

            ChwPostpartumEncounter ->
                "chw-postpartum"
