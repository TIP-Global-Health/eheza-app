module Backend.Menu.Decoder exposing (decodeMenuData)

import Backend.Decoder exposing (decodeSite)
import Backend.Menu.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
