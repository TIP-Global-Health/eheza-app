module Backend.HIVEncounter.Decoder exposing (decodeHIVEncounter)

import Backend.HIVEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeHIVEncounter : Decoder HIVEncounter
decodeHIVEncounter =
    succeed HIVEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing
