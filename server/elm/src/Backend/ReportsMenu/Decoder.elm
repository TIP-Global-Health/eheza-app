module Backend.ReportsMenu.Decoder exposing (decodeMenuData)

import Backend.Components.Decoder exposing (decodeHealthCenterData, decodeMenuScope)
import Backend.Decoder exposing (decodeSite)
import Backend.ReportsMenu.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)
        |> optional "scope" (maybe decodeMenuScope) Nothing
