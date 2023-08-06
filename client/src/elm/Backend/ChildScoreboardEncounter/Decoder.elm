module Backend.ChildScoreboardEncounter.Decoder exposing (decodeChildScoreboardEncounter)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeEverySet)


decodeChildScoreboardEncounter : Decoder ChildScoreboardEncounter
decodeChildScoreboardEncounter =
    succeed ChildScoreboardEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing