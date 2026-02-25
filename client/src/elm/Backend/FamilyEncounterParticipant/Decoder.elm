module Backend.FamilyEncounterParticipant.Decoder exposing (..)

import Backend.FamilyEncounterParticipant.Model exposing (..)
import Backend.FamilyEncounterParticipant.Utils exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeFamilyEncounterParticipant : Decoder FamilyEncounterParticipant
decodeFamilyEncounterParticipant =
    succeed FamilyEncounterParticipant
        |> required "person" decodeEntityUuid
        |> required "family_encounter_type" decodeFamilyEncounterType
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "deleted" bool
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeFamilyEncounterType : Decoder FamilyEncounterType
decodeFamilyEncounterType =
    string
        |> andThen
            (\s ->
                familyEncounterTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized EncounterType" |> fail)
            )
