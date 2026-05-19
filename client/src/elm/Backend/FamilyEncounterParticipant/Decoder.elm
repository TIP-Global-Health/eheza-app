module Backend.FamilyEncounterParticipant.Decoder exposing (decodeFamilyEncounterParticipant)

import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant, FamilyEncounterType)
import Backend.FamilyEncounterParticipant.Utils exposing (familyEncounterTypeFromString)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, bool, fail, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
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
