module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import Backend.Dashboard.Model exposing (DashboardStats)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeDashboardStats : Decoder DashboardStats
decodeDashboardStats =
    succeed DashboardStats
        |> required "total_measurements" decodeInt
