module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, Nutrition, NutritionStatus(..), NutritionValue, ParticipantStats, Periods, TotalBeneficiaries)
import Backend.Measurement.Decoder exposing (decodeFamilyPlanningSign)
import Backend.Person.Decoder exposing (decodeGender)
import Dict as LegacyDict
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeDashboardStats : Decoder DashboardStats
decodeDashboardStats =
    succeed DashboardStats
        |> required "case_management" (list decodeCaseManagement)
        |> required "children_beneficiaries" (list decodePeopleStats)
        |> required "completed_program" (list decodeParticipantStats)
        |> required "family_planning" (list decodeFamilyPlanningStats)
        |> optional "good_nutrition" (nullable decodeGoodNutrition) Nothing
        |> required "malnourished_beneficiaries" (list decodeMalnourishedStats)
        |> required "missed_sessions" (list decodeParticipantStats)
        |> optional "total_beneficiaries" (nullable decodeTotalBeneficiariesDict) Nothing
        |> optional "total_beneficiaries_incidence" (nullable decodeTotalBeneficiariesDict) Nothing
        |> optional "total_encounters" (nullable decodePeriods) Nothing


decodeTotalBeneficiariesDict : Decoder (Dict Int TotalBeneficiaries)
decodeTotalBeneficiariesDict =
    dict decodeTotalBeneficiaries
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) -> ( Maybe.withDefault 1 (String.toInt k), v ))
                    |> Dict.fromList
                    |> succeed
            )


decodeCaseManagement : Decoder CaseManagement
decodeCaseManagement =
    succeed CaseManagement
        |> required "name" string
        |> required "nutrition" decodeCaseNutrition


decodeCaseNutrition : Decoder CaseNutrition
decodeCaseNutrition =
    succeed CaseNutrition
        |> required "stunting" decodeNutritionValueDict
        |> required "underweight" decodeNutritionValueDict
        |> required "wasting" decodeNutritionValueDict
        |> required "muac" decodeNutritionValueDict


decodeNutritionValueDict : Decoder (Dict Int NutritionValue)
decodeNutritionValueDict =
    dict decodeNutritionValue
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) -> ( Maybe.withDefault 1 (String.toInt k), v ))
                    |> Dict.fromList
                    |> succeed
            )


decodeNutritionValue : Decoder NutritionValue
decodeNutritionValue =
    succeed NutritionValue
        |> required "class" decodeNutritionStatus
        |> required "value" string


decodeNutritionStatus : Decoder NutritionStatus
decodeNutritionStatus =
    string
        |> andThen
            (\s ->
                case s of
                    "neutral" ->
                        succeed Neutral

                    "good_nutrition" ->
                        succeed Good

                    "moderate_nutrition" ->
                        succeed Moderate

                    "severe_nutrition" ->
                        succeed Severe

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized nutrition status."
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


decodePeopleStats : Decoder ChildrenBeneficiariesStats
decodePeopleStats =
    succeed ChildrenBeneficiariesStats
        |> required "field_gender" decodeGender
        |> required "field_birth_date" decodeYYYYMMDD
        |> required "created" decodeYYYYMMDD
        |> required "name" string
        |> required "mother_name" string
        |> optional "phone_number" (nullable string) Nothing


decodeParticipantStats : Decoder ParticipantStats
decodeParticipantStats =
    succeed ParticipantStats
        |> required "name" string
        |> required "gender" decodeGender
        |> required "birth_date" decodeYYYYMMDD
        |> required "mother_name" string
        |> optional "phone_number" (nullable string) Nothing
        |> required "expected_date" decodeYYYYMMDD


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
        |> required "field_birth_date" decodeYYYYMMDD
        |> required "field_gender" decodeGender
        |> required "field_zscore_age" float


decodePeriods : Decoder Periods
decodePeriods =
    succeed Periods
        |> required "last_year" decodeInt
        |> required "this_year" decodeInt
