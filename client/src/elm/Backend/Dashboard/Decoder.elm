module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import Backend.Dashboard.Model exposing (DashboardStats, PeopleStats)
import Backend.Person.Decoder exposing (decodeGender)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeDashboardStats : Decoder DashboardStats
decodeDashboardStats =
    succeed DashboardStats
        |> required "people" (list decodePeopleStats)


decodePeopleStats : Decoder PeopleStats
decodePeopleStats =
    succeed PeopleStats
        |> required "field_gender" decodeGender
        |> required "field_birth_date" decodeYYYYMMDD
        |> required "created" decodeYYYYMMDD
