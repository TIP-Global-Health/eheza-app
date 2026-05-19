module Backend.CompletionMenu.Decoder exposing (decodeMenuData)

import Backend.CompletionMenu.Model exposing (MenuData)
import Backend.Components.Decoder exposing (decodeHealthCenterData, decodeMenuScope)
import Backend.Decoder exposing (decodeSite)
import Json.Decode exposing (Decoder, list, maybe, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)
        |> optional "scope" (maybe decodeMenuScope) Nothing
