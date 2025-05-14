module Backend.ReportsMenu.Decoder exposing (decodeMenuData)

import Backend.Components.Decoder exposing (decodeHealthCenterData)
import Backend.Decoder exposing (decodeSite)
import Backend.ReportsMenu.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, fail, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)
        |> optional "scope" (maybe decodeMenuScope) Nothing


decodeMenuScope : Decoder MenuScope
decodeMenuScope =
    string
        |> andThen
            (\scope ->
                case scope of
                    "health_centers" ->
                        succeed ScopeHealthCenters

                    _ ->
                        fail <| scope ++ " is unknown MenuScope type"
            )
