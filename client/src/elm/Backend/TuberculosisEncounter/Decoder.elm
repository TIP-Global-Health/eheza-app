module Backend.TuberculosisEncounter.Decoder exposing (decodeTuberculosisEncounter)

import Backend.TuberculosisEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, bool, nullable, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodeTuberculosisEncounter : Decoder TuberculosisEncounter
decodeTuberculosisEncounter =
    succeed TuberculosisEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "deleted" (decodeWithFallback False bool)
        |> optional "shard" (nullable decodeEntityUuid) Nothing
