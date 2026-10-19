module Backend.Dashboard.Decoder exposing (decodeDashboardData)

import Backend.Components.Decoder exposing (decodeMenuData)
import Backend.Dashboard.Model exposing (DashboardData)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline exposing (custom, optional)


decodeDashboardData : Decoder DashboardData
decodeDashboardData =
    succeed DashboardData
        |> optional "first_cohort_date" (nullable decodeYYYYMMDD) Nothing
        |> custom decodeMenuData
