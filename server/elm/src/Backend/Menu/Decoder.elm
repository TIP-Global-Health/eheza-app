module Backend.Menu.Decoder exposing (decodeMenuData)

import Backend.Menu.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite


decodeSite : Decoder Site
decodeSite =
    string
        |> andThen (siteFromString >> succeed)


siteFromString : String -> Site
siteFromString str =
    case String.toLower str of
        "rwanda" ->
            SiteRwanda

        "burundi" ->
            SiteBurundi

        _ ->
            SiteUnknown
