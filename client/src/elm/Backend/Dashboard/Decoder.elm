module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, Nutrition, Periods, TotalBeneficiaries)
import Backend.Measurement.Decoder exposing (decodeFamilyPlanningSign)
import Backend.Person.Decoder exposing (decodeGender)
import Date exposing (Month)
import Dict as LegacyDict
import Gizra.Json exposing (decodeFloat, decodeInt)
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
        |> required "total_beneficiaries" decodeTotalBeneficiariesDict
        |> required "total_beneficiaries_max" decodeFloat
        |> required "total_encounters" decodePeriods


decodeTotalBeneficiariesDict : Decoder (Dict Month TotalBeneficiaries)
decodeTotalBeneficiariesDict =
    dict decodeTotalBeneficiaries
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) -> ( stringToMonth k, v ))
                    |> Dict.fromList
                    |> succeed
            )


decodeTotalBeneficiaries : Decoder TotalBeneficiaries
decodeTotalBeneficiaries =
    succeed TotalBeneficiaries
        |> required "stunting" decodeBeneficiaries
        |> required "underweight" decodeBeneficiaries
        |> required "wasting" decodeBeneficiaries
        |> required "muac" decodeBeneficiaries


decodeBeneficiaries : Decoder Nutrition
decodeBeneficiaries =
    succeed Nutrition
        |> required "severe_nutrition" decodeInt
        |> required "moderate_nutrition" decodeInt


stringToMonth : String -> Month
stringToMonth m =
    case m of
        "jan" ->
            Jan

        "feb" ->
            Feb

        "mar" ->
            Mar

        "apr" ->
            Apr

        "may" ->
            May

        "jun" ->
            Jun

        "jul" ->
            Jul

        "aug" ->
            Aug

        "sep" ->
            Sep

        "oct" ->
            Oct

        "nov" ->
            Nov

        "dec" ->
            Dec

        _ ->
            Jan


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
