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
import Gizra.Json exposing (decodeFloat, decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Pages.AcuteIllnessEncounter.Utils exposing (compareAcuteIllnessEncounterDataDesc)
import Restful.Endpoint exposing (decodeEntityUuid, toEntityUuid)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


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
        |> required "stunting" (decodeNutritionValueDict decodeZScoreNutritionValue)
        |> required "underweight" (decodeNutritionValueDict decodeZScoreNutritionValue)
        |> required "wasting" (decodeNutritionValueDict decodeZScoreNutritionValue)
        |> required "muac" (decodeNutritionValueDict decodeMuacNutritionValue)
        |> required "nutrition_signs" (decodeNutritionValueDict decodeZScoreNutritionValue)


decodeNutritionValueDict : Decoder NutritionValue -> Decoder (Dict Int NutritionValue)
decodeNutritionValueDict decoder =
    dict (decodeWithFallback (NutritionValue Neutral "X") decoder)
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


decodeZScoreNutritionValue : Decoder NutritionValue
decodeZScoreNutritionValue =
    float
        |> andThen
            (\value ->
                if value <= -3 then
                    succeed <| NutritionValue Severe (String.fromFloat value)

                else if value <= -2 then
                    succeed <| NutritionValue Moderate (String.fromFloat value)

                else
                    succeed <| NutritionValue Good (String.fromFloat value)
            )


decodeMuacNutritionValue : Decoder NutritionValue
decodeMuacNutritionValue =
    decodeFloat
        |> andThen
            (\value ->
                if value <= 11.5 then
                    succeed <| NutritionValue Severe (String.fromFloat value)

                else if value <= 12.5 then
                    succeed <| NutritionValue Moderate (String.fromFloat value)

                else
                    succeed <| NutritionValue Good (String.fromFloat value)
            )


decodeNutritionValue : Decoder NutritionValue
decodeNutritionValue =
    succeed NutritionValue
        |> required "c" decodeNutritionStatus
        |> required "v" string


decodeNutritionStatus : Decoder NutritionStatus
decodeNutritionStatus =
    string
        |> andThen
            (\s ->
                case s of
                    "0" ->
                        succeed Neutral

                    "1" ->
                        succeed Good

                    "2" ->
                        succeed Moderate

                    "3" ->
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
        |> required "sequence_number" (decodeWithFallback 1 decodeInt)
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
