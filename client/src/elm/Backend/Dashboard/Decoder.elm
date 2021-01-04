module Backend.Dashboard.Decoder exposing (decodeDashboardStats)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (..)
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
        |> required "case_management" decodeCaseManagementData
        |> required "children_beneficiaries" (list decodePeopleStats)
        |> required "completed_program" (list decodeParticipantStats)
        |> required "family_planning" (list decodeFamilyPlanningStats)
        |> required "missed_sessions" (list decodeParticipantStats)
        |> required "total_encounters" decodeTotalEncounters
        |> required "stats_cache_hash" string


decodeCaseManagementData : Decoder CaseManagementData
decodeCaseManagementData =
    succeed CaseManagementData
        |> required "this_year" decodeCaseManagementDataForYear
        |> required "last_year" decodeCaseManagementDataForYear


decodeCaseManagementDataForYear : Decoder (Dict ProgramType (List CaseManagement))
decodeCaseManagementDataForYear =
    dict (list decodeCaseManagement)
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) ->
                            ( programTypeFromString k, v )
                        )
                    |> Dict.fromList
                    |> succeed
            )


decodeCaseManagement : Decoder CaseManagement
decodeCaseManagement =
    succeed CaseManagement
        |> required "id" int
        |> required "name" string
        |> required "birth_date" decodeYYYYMMDD
        |> required "gender" decodeGender
        |> required "nutrition" decodeCaseNutrition


decodeCaseNutrition : Decoder CaseNutrition
decodeCaseNutrition =
    succeed CaseNutrition
        |> required "stunting" decodeNutritionValueDict
        |> required "underweight" decodeNutritionValueDict
        |> required "wasting" decodeNutritionValueDict
        |> required "muac" decodeNutritionValueDict
        |> required "nutrition_signs" decodeNutritionValueDict


decodeNutritionValueDict : Decoder (Dict Int NutritionValue)
decodeNutritionValueDict =
    dict decodeNutritionValue
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) ->
                            ( Maybe.withDefault 1 (String.toInt k), v )
                        )
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
        |> required "graduation_date" decodeYYYYMMDD


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


decodeTotalEncounters : Decoder (Dict ProgramType Periods)
decodeTotalEncounters =
    dict decodePeriods
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) ->
                            ( programTypeFromString k, v )
                        )
                    |> Dict.fromList
                    |> succeed
            )


decodePeriods : Decoder Periods
decodePeriods =
    succeed Periods
        |> required "last_year" decodeInt
        |> required "this_year" decodeInt


programTypeFromString : String -> ProgramType
programTypeFromString string =
    case string of
        "achi" ->
            ProgramAchi

        "fbf" ->
            ProgramFbf

        "individual" ->
            ProgramIndividual

        "pmtct" ->
            ProgramPmtct

        "sorwathe" ->
            ProgramSorwathe

        _ ->
            ProgramUnknown
