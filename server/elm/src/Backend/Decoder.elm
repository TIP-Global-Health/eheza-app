module Backend.Decoder exposing (decodeSite, decodeWithFallback)

import App.Types exposing (Site(..))
import Backend.ScoreboardMenu.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, oneOf, string, succeed)
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


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
