module Backend.CompletionMenu.Decoder exposing (decodeMenuData)

import Backend.CompletionMenu.Model exposing (..)
import Backend.Components.Decoder exposing (decodeHealthCenterData)
import Backend.Decoder exposing (decodeSite)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)
