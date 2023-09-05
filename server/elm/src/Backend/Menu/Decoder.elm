module Backend.Menu.Decoder exposing (decodeMenuData)

import AssocList as Dict exposing (Dict)
import Backend.Menu.Model exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, float, int, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Maybe.Extra exposing (isNothing)


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
