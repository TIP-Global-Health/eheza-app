module Backend.ScoreboardMenu.Decoder exposing (decodeMenuData)

import Backend.Decoder exposing (decodeSite)
import Backend.ScoreboardMenu.Model exposing (..)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
