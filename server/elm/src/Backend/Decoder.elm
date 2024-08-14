module Backend.Decoder exposing (decodeSite)

import App.Types exposing (Site(..))
import Backend.ScoreboardMenu.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, string, succeed)
import Json.Decode.Pipeline exposing (required)


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
