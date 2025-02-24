module Backend.ReportsMenu.Decoder exposing (decodeMenuData)

import Backend.Decoder exposing (decodeSite)
import Backend.ReportsMenu.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)


decodeHealthCenterData : Decoder HealthCenterData
decodeHealthCenterData =
    succeed HealthCenterData
        |> required "id" decodeInt
        |> required "name" string
