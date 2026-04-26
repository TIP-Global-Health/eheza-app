module Backend.FamilyEncounterParticipant.Encoder exposing (encodeFamilyEncounterParticipant)

import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant, FamilyEncounterType)
import Backend.FamilyEncounterParticipant.Utils exposing (familyEncounterTypeToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
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
