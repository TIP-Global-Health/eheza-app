module Backend.Dashboard.Decoder exposing (decodeDashboardRaw)

import Backend.Dashboard.Model exposing (DashboardRaw)
import Backend.HealthCenter.Decoder exposing (decodeHealthCenter)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeDashboardRaw : Decoder DashboardRaw
decodeDashboardRaw =
    succeed DashboardRaw
        |> required "computed_property" string
        |> custom decodeHealthCenter
