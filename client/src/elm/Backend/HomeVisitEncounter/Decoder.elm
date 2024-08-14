module Backend.HomeVisitEncounter.Decoder exposing (decodeHomeVisitEncounter)

import Backend.HomeVisitEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeHomeVisitEncounter : Decoder HomeVisitEncounter
decodeHomeVisitEncounter =
    succeed HomeVisitEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing
