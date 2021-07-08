module Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessDiagnosis)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model exposing (..)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeDeliveryLocation, decodeIndividualEncounterParticipantOutcome)
import Backend.Measurement.Decoder
    exposing
        ( decodeCall114Sign
        , decodeDangerSign
        , decodeFamilyPlanningSign
        , decodeHCContactSign
        , decodeHCRecommendation
        , decodeIsolationSign
        , decodeRecommendation114
        , decodeSendToHCSign
        )
import Backend.Measurement.Model
    exposing
        ( Call114Sign(..)
        , DangerSign(..)
        , HCContactSign(..)
        , HCRecommendation(..)
        , IsolationSign(..)
        , Recommendation114(..)
        , SendToHCSign(..)
        )
import Backend.Person.Decoder exposing (decodeGender)
import Dict as LegacyDict
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Pages.AcuteIllnessEncounter.Utils exposing (compareAcuteIllnessEncounterDataDesc)
import Restful.Endpoint exposing (decodeEntityUuid, toEntityUuid)
import Utils.Json exposing (decodeEverySet)


decodeDashboardStatsRaw : Decoder DashboardStatsRaw
decodeDashboardStatsRaw =
    succeed DashboardStatsRaw
        |> required "case_management" decodeCaseManagementData
        |> required "children_beneficiaries" decodeChildrenBeneficiariesData
        |> required "completed_program" (list decodeParticipantStats)
        |> required "family_planning" (list decodeFamilyPlanningStats)
        |> required "missed_sessions" (list decodeParticipantStats)
        |> required "total_encounters" decodeTotalEncountersData
        |> required "acute_illness_data" (list decodeAcuteIllnessDataItem)
        |> required "prenatal_data" (list decodePrenatalDataItem)
        |> required "villages_with_residents" decodeVillagesWithResidents
        |> required "timestamp" string
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
        |> required "id" decodeInt
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


decodeChildrenBeneficiariesData : Decoder (Dict ProgramType (List ChildrenBeneficiariesStats))
decodeChildrenBeneficiariesData =
    dict (list decodeChildrenBeneficiariesStats)
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


decodeChildrenBeneficiariesStats : Decoder ChildrenBeneficiariesStats
decodeChildrenBeneficiariesStats =
    succeed ChildrenBeneficiariesStats
        |> required "id" decodeInt
        |> required "gender" decodeGender
        |> required "birth_date" decodeYYYYMMDD
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


decodeTotalEncountersData : Decoder TotalEncountersData
decodeTotalEncountersData =
    succeed TotalEncountersData
        |> required "global" decodeTotalEncounters
        |> required "villages" decodeTotalEncountersForVillages


decodeTotalEncountersForVillages : Decoder (Dict VillageId (Dict ProgramType Periods))
decodeTotalEncountersForVillages =
    oneOf
        [ decodeTotalEncountersForVillages_
        , succeed Dict.empty
        ]


decodeTotalEncountersForVillages_ : Decoder (Dict VillageId (Dict ProgramType Periods))
decodeTotalEncountersForVillages_ =
    dict decodeTotalEncounters
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) ->
                            ( toEntityUuid k, v )
                        )
                    |> Dict.fromList
                    |> succeed
            )


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

        "chw" ->
            ProgramChw

        _ ->
            ProgramUnknown


decodeVillagesWithResidents : Decoder (Dict VillageId (List Int))
decodeVillagesWithResidents =
    oneOf
        [ decodeVillagesWithResidents_
        , succeed Dict.empty
        ]


decodeVillagesWithResidents_ : Decoder (Dict VillageId (List Int))
decodeVillagesWithResidents_ =
    dict (list int)
        |> andThen
            (\dict ->
                LegacyDict.toList dict
                    |> List.map
                        (\( k, v ) ->
                            ( toEntityUuid k, v )
                        )
                    |> Dict.fromList
                    |> succeed
            )


decodeAcuteIllnessDataItem : Decoder AcuteIllnessDataItem
decodeAcuteIllnessDataItem =
    succeed AcuteIllnessDataItem
        |> required "id" decodeInt
        |> required "created" decodeYYYYMMDD
        |> hardcoded NoAcuteIllnessDiagnosis
        |> required "date_concluded" (nullable decodeYYYYMMDD)
        |> required "outcome" (nullable decodeIndividualEncounterParticipantOutcome)
        |> required "encounters" (list decodeAcuteIllnessEncounterDataItem)
        |> Json.Decode.map
            (\item ->
                let
                    orderedEncounters =
                        List.sortWith compareAcuteIllnessEncounterDataDesc item.encounters

                    resolvedDiagnosis =
                        List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis) orderedEncounters
                            |> List.head
                            |> Maybe.map .diagnosis
                            |> Maybe.withDefault NoAcuteIllnessDiagnosis
                in
                { item | diagnosis = resolvedDiagnosis, encounters = orderedEncounters }
            )


decodeAcuteIllnessEncounterDataItem : Decoder AcuteIllnessEncounterDataItem
decodeAcuteIllnessEncounterDataItem =
    succeed AcuteIllnessEncounterDataItem
        |> required "start_date" decodeYYYYMMDD
        |> required "sequence_number" decodeInt
        |> required "diagnosis" decodeAcuteIllnessDiagnosis
        |> required "fever" bool
        |> required "isolation" (decodeEverySet (decodeWithFallback NoIsolationSigns decodeIsolationSign))
        |> required "send_to_hc" (decodeEverySet (decodeWithFallback NoSendToHCSigns decodeSendToHCSign))
        |> required "call_114" (decodeEverySet (decodeWithFallback NoCall114Signs decodeCall114Sign))
        |> required "recommendation_114" (decodeEverySet (decodeWithFallback NoneOtherRecommendation114 decodeRecommendation114))
        |> required "contact_hc" (decodeEverySet (decodeWithFallback NoHCContactSigns decodeHCContactSign))
        |> required "recommendation_hc" (decodeEverySet (decodeWithFallback HCRecommendationNotApplicable decodeHCRecommendation))


decodePrenatalDataItem : Decoder PrenatalDataItem
decodePrenatalDataItem =
    succeed PrenatalDataItem
        |> required "id" decodeInt
        |> required "created" decodeYYYYMMDD
        |> required "expected_date_concluded" (nullable decodeYYYYMMDD)
        |> required "date_concluded" (nullable decodeYYYYMMDD)
        |> required "outcome" (nullable decodeIndividualEncounterParticipantOutcome)
        |> required "delivery_location" (nullable decodeDeliveryLocation)
        |> required "encounters" (list decodePrenatalEncounterDataItem)


decodePrenatalEncounterDataItem : Decoder PrenatalEncounterDataItem
decodePrenatalEncounterDataItem =
    succeed PrenatalEncounterDataItem
        |> required "start_date" decodeYYYYMMDD
        |> required "danger_signs" (decodeEverySet (decodeWithFallback NoDangerSign decodeDangerSign))


decodeDangerSignWithFallback : Decoder DangerSign
decodeDangerSignWithFallback =
    decodeWithFallback NoDangerSign decodeDangerSign


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
