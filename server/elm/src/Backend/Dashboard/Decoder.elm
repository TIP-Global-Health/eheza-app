module Backend.Dashboard.Decoder exposing (decodeDashboardData)

import Backend.Components.Decoder exposing (decodeHealthCenterData)
import Backend.Dashboard.Model exposing (DashboardData)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, list, nullable, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeDashboardData : Decoder DashboardData
decodeDashboardData =
    succeed DashboardData
        |> optional "first_cohort_date" (nullable decodeYYYYMMDD) Nothing
        |> required "health_centers" (list decodeHealthCenterData)
