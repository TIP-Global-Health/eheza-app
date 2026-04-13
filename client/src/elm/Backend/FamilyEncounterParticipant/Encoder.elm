module Backend.FamilyEncounterParticipant.Encoder exposing (..)

import Backend.FamilyEncounterParticipant.Model exposing (..)
import Backend.FamilyEncounterParticipant.Utils exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


encodeFamilyEncounterParticipant : FamilyEncounterParticipant -> List ( String, Value )
encodeFamilyEncounterParticipant data =
    [ ( "person", encodeEntityUuid data.person )
    , ( "family_encounter_type", encodeFamilyEncounterType data.encounterType )
    , ( "expected"
      , object
            [ ( "value", encodeYYYYMMDD data.startDate )
            , ( "value2", maybe encodeYYYYMMDD data.endDate )
            ]
      )
    , ( "deleted", bool data.deleted )
    , ( "type", string "family_participant" )
    ]
        ++ encodeIfSet "shard" data.shard encodeEntityUuid


encodeFamilyEncounterType : FamilyEncounterType -> Value
encodeFamilyEncounterType type_ =
    familyEncounterTypeToString type_ |> string
