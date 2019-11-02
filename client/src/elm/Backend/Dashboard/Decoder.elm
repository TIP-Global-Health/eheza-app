module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats)
import Backend.Measurement.Decoder exposing (decodeFamilyPlanningSign)
import Backend.Person.Decoder exposing (decodeGender)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeDashboardStats : Decoder DashboardStats
decodeDashboardStats =
    succeed DashboardStats
        |> required "children_beneficiaries" (list decodePeopleStats)
        |> required "family_planning" (list decodeFamilyPlanningStats)


decodePeopleStats : Decoder ChildrenBeneficiariesStats
decodePeopleStats =
    succeed ChildrenBeneficiariesStats
        |> required "field_gender" decodeGender
        |> required "field_birth_date" decodeYYYYMMDD
        |> required "created" decodeYYYYMMDD


decodeFamilyPlanningStats : Decoder FamilyPlanningStats
decodeFamilyPlanningStats =
    succeed FamilyPlanningStats
        |> required "created" decodeYYYYMMDD
        |> required "signs" (list decodeFamilyPlanningSign)
