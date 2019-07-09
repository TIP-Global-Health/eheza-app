module Backend.PrenatalParticipant.Decoder exposing (decodePrenatalParticipant)

import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodePrenatalParticipant : Decoder PrenatalParticipant
decodePrenatalParticipant =
    succeed PrenatalParticipant
        |> required "person" decodeEntityUuid
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing
