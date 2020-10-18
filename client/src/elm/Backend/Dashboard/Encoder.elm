module Backend.Dashboard.Encoder exposing (encodeDashboardStats)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model
    exposing
        ( CaseManagement
        , CaseNutrition
        , ChildrenBeneficiariesStats
        , DashboardStats
        , FamilyPlanningStats
        , GoodNutrition
        , Nutrition
        , NutritionStatus(..)
        , NutritionValue
        , ParticipantStats
        , Periods
        , TotalBeneficiaries
        )
import Backend.Measurement.Encoder exposing (encodeFamilyPlanningSign)
import Backend.Person.Encoder exposing (encodeGender)
import Dict as LegacyDict
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)


encodeDashboardStats : DashboardStats -> List ( String, Value )
encodeDashboardStats stats =
    [ encodeCasesManagements stats.caseManagement
    , encodeChildrenBeneficiaries stats.childrenBeneficiaries
    , encodeCompletedPrograms stats.completedPrograms
    , encodeFamilyPlanning stats.familyPlanning
    , encodeMissedSessions stats.missedSessions
    , encodeTotalEncounters stats.totalEncounters
    , ( "stats_cache_hash", string stats.cacheHash )
    ]
        ++ (encodeGoodNutrition stats.maybeGoodNutrition
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )


encodeCasesManagements : List CaseManagement -> ( String, Value )
encodeCasesManagements statsList =
    ( "case_management", list (encodeCaseManagement >> object) statsList )


encodeCaseManagement : CaseManagement -> List ( String, Value )
encodeCaseManagement caseManagement =
    [ ( "id", int caseManagement.identifier )
    , ( "name", string caseManagement.name )
    , ( "birth_date", encodeYYYYMMDD caseManagement.birthDate )
    , ( "gender", encodeGender caseManagement.gender )
    , ( "nutrition", object <| encodeCaseNutrition caseManagement.nutrition )
    ]


encodeCaseNutrition : CaseNutrition -> List ( String, Value )
encodeCaseNutrition caseNutrition =
    [ ( "stunting", dict String.fromInt (encodeNutritionValue >> object) (dictToLegacyDict caseNutrition.stunting) )
    , ( "underweight", dict String.fromInt (encodeNutritionValue >> object) (dictToLegacyDict caseNutrition.underweight) )
    , ( "wasting", dict String.fromInt (encodeNutritionValue >> object) (dictToLegacyDict caseNutrition.wasting) )
    , ( "muac", dict String.fromInt (encodeNutritionValue >> object) (dictToLegacyDict caseNutrition.muac) )
    ]


dictToLegacyDict : Dict comparable v -> LegacyDict.Dict comparable v
dictToLegacyDict dict =
    Dict.toList dict
        |> LegacyDict.fromList


encodeNutritionValue : NutritionValue -> List ( String, Value )
encodeNutritionValue value =
    [ ( "class", encodeNutritionStatus value.class )
    , ( "value", string value.value )
    ]


encodeNutritionStatus : NutritionStatus -> Value
encodeNutritionStatus status =
    string <|
        case status of
            Neutral ->
                "neutral"

            Good ->
                "good_nutrition"

            Moderate ->
                "moderate_nutrition"

            Severe ->
                "severe_nutrition"


encodeChildrenBeneficiaries : List ChildrenBeneficiariesStats -> ( String, Value )
encodeChildrenBeneficiaries statsList =
    ( "children_beneficiaries", list (encodeChildrenBeneficiariesStats >> object) statsList )


encodeChildrenBeneficiariesStats : ChildrenBeneficiariesStats -> List ( String, Value )
encodeChildrenBeneficiariesStats stats =
    [ ( "field_birth_dategender", encodeGender stats.gender )
    , ( "field_birth_date", encodeYYYYMMDD stats.birthDate )
    , ( "created", encodeYYYYMMDD stats.memberSince )
    , ( "mother_name", string stats.motherName )
    , ( "phone_number", maybe string stats.phoneNumber )
    , ( "graduation_date", encodeYYYYMMDD stats.graduationDate )
    ]


encodeCompletedPrograms : List ParticipantStats -> ( String, Value )
encodeCompletedPrograms statsList =
    ( "completed_program", list (encodeParticipantStats >> object) statsList )


encodeFamilyPlanning : List FamilyPlanningStats -> ( String, Value )
encodeFamilyPlanning statsList =
    ( "family_planning", list (encodeFamilyPlanningStats >> object) statsList )


encodeFamilyPlanningStats : FamilyPlanningStats -> List ( String, Value )
encodeFamilyPlanningStats stats =
    [ ( "created", encodeYYYYMMDD stats.created )
    , ( "signs", list encodeFamilyPlanningSign stats.signs )
    ]


encodeGoodNutrition : Maybe GoodNutrition -> Maybe ( String, Value )
encodeGoodNutrition mGoodNutrition =
    mGoodNutrition
        |> Maybe.map
            (\goodNutrition ->
                ( "good_nutrition"
                , object
                    [ encodePeriodsAs "all" goodNutrition.all
                    , encodePeriodsAs "good" goodNutrition.good
                    ]
                )
            )


encodeMissedSessions : List ParticipantStats -> ( String, Value )
encodeMissedSessions statsList =
    ( "missed_sessions", list (encodeParticipantStats >> object) statsList )


encodeParticipantStats : ParticipantStats -> List ( String, Value )
encodeParticipantStats stats =
    [ ( "name", string stats.name )
    , ( "gender", encodeGender stats.gender )
    , ( "birth_date", encodeYYYYMMDD stats.birthDate )
    , ( "mother_name", string stats.motherName )
    , ( "phone_number", maybe string stats.phoneNumber )
    , ( "expected_date", encodeYYYYMMDD stats.expectedDate )
    ]


encodeTotalEncounters : Periods -> ( String, Value )
encodeTotalEncounters periods =
    encodePeriodsAs "total_encounters" periods


encodePeriodsAs : String -> Periods -> ( String, Value )
encodePeriodsAs fieldName periods =
    ( fieldName, object <| encodePeriods periods )


encodePeriods : Periods -> List ( String, Value )
encodePeriods periods =
    [ ( "last_year", int periods.lastYear )
    , ( "this_year", int periods.thisYear )
    ]
