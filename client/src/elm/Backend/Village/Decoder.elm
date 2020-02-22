module Backend.Village.Decoder exposing (decodeVillage)

import Backend.Village.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeVillage : Decoder Village
decodeVillage =
    succeed Village
        |> required "health_center" decodeEntityUuid
        |> required "label" string
        |> required "province" string
        |> required "district" string
        |> required "sector" string
        |> required "cell" string
        |> required "village" string
