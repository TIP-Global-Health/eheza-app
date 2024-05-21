module Backend.ReportsMenu.Decoder exposing (decodeMenuData)

import Backend.Decoder exposing (decodeSite)
import Backend.ReportsMenu.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
