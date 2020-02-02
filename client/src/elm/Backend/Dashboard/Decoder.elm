module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import AssocList as Dict
import Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, Periods, TotalBeneficiaries)
import Backend.Measurement.Decoder exposing (decodeFamilyPlanningSign)
import Backend.Person.Decoder exposing (decodeGender)
import Date exposing (Month)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Month(..))


decodeDashboardStats : Decoder DashboardStats
decodeDashboardStats =
    succeed DashboardStats
        |> required "children_beneficiaries" (list decodePeopleStats)
        |> required "family_planning" (list decodeFamilyPlanningStats)
        |> required "good_nutrition" decodeGoodNutrition
        |> required "malnourished_beneficiaries" (list decodeMalnourishedStats)
        --|> required "total_beneficiaries" decodeTotalBeneficiaries
        |> required "total_encounters" decodePeriods


decodeMonth : Decoder Month
decodeMonth =
    andThen
        (\m ->
            case m of
                "jan" ->
                    succeed Jan

                "feb" ->
                    succeed Feb

                "mar" ->
                    succeed Mar

                "apr" ->
                    succeed Apr

                "may" ->
                    succeed May

                "jun" ->
                    succeed Jun

                "jul" ->
                    succeed Jul

                "aug" ->
                    succeed Aug

                "sep" ->
                    succeed Sep

                "oct" ->
                    succeed Oct

                "nov" ->
                    succeed Nov

                "dec" ->
                    succeed Dec

                _ ->
                    fail <|
                        m
                            ++ " is not a recognized Month"
        )
        string



--decodeTotalBeneficiaries : Decoder a -> TotalBeneficiaries
--decodeTotalBeneficiaries =
--    succeed TotalBeneficiaries |> Dict.empty


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


decodeGoodNutrition : Decoder GoodNutrition
decodeGoodNutrition =
    succeed GoodNutrition
        |> required "all" decodePeriods
        |> required "good" decodePeriods


decodeMalnourishedStats : Decoder MalnourishedStats
decodeMalnourishedStats =
    succeed MalnourishedStats
        |> required "created" decodeYYYYMMDD
        |> required "field_gender" decodeGender
        |> required "field_zscore_age" float


decodePeriods : Decoder Periods
decodePeriods =
    succeed Periods
        |> required "last_year" decodeInt
        |> required "this_year" decodeInt
