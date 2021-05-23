module Backend.Dashboard.Encoder exposing (encodeDashboardStats)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (..)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Encoder exposing (encodeDeliveryLocation, encodeIndividualEncounterParticipantOutcome)
import Backend.Measurement.Encoder exposing (encodeFamilyPlanningSign)
import Backend.Person.Encoder exposing (encodeGender)
import Dict as LegacyDict
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (fromEntityUuid)


encodeDashboardStats : DashboardStats -> List ( String, Value )
encodeDashboardStats stats =
    [ encodeCasesManagementData stats.caseManagement
    , encodeChildrenBeneficiaries stats.childrenBeneficiaries
    , encodeCompletedPrograms stats.completedPrograms
    , encodeFamilyPlanning stats.familyPlanning
    , encodeMissedSessions stats.missedSessions
    , encodeTotalEncountersData stats.totalEncounters
    , encodePrenatalData stats.prenatalData
    , encodeVillagesWithResidents stats.villagesWithResidents
    , ( "timestamp", string stats.timestamp )
    , ( "stats_cache_hash", string stats.cacheHash )
    ]


encodeCasesManagementData : CaseManagementData -> ( String, Value )
encodeCasesManagementData data =
    ( "case_management"
    , object
        [ ( "this_year", encodeCasesManagementForYear data.thisYear )
        , ( "last_year", encodeCasesManagementForYear data.lastYear )
        ]
    )


encodeCasesManagementForYear : Dict ProgramType (List CaseManagement) -> Value
encodeCasesManagementForYear dict =
    Dict.toList dict
        |> List.map
            (\( programType, casesList ) ->
                ( programTypeToString programType, list (encodeCaseManagement >> object) casesList )
            )
        |> object


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
    , ( "nutrition_signs", dict String.fromInt (encodeNutritionValue >> object) (dictToLegacyDict caseNutrition.nutritionSigns) )
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
    [ ( "name", string stats.name )
    , ( "field_gender", encodeGender stats.gender )
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


encodeTotalEncountersData : TotalEncountersData -> ( String, Value )
encodeTotalEncountersData data =
    ( "total_encounters"
    , object
        [ ( "global", encodeTotalEncounters data.global )
        , ( "villages", encodeTotalEncountersForVillages data.villages )
        ]
    )


encodeTotalEncountersForVillages : Dict VillageId (Dict ProgramType Periods) -> Value
encodeTotalEncountersForVillages dict =
    Dict.toList dict
        |> List.map
            (\( villageId, totalEncounters ) ->
                ( fromEntityUuid villageId, encodeTotalEncounters totalEncounters )
            )
        |> object


encodeTotalEncounters : Dict ProgramType Periods -> Value
encodeTotalEncounters dict =
    Dict.toList dict
        |> List.map
            (\( programType, periods ) ->
                encodePeriodsAs (programTypeToString programType) periods
            )
        |> object


programTypeToString : ProgramType -> String
programTypeToString programType =
    case programType of
        ProgramAchi ->
            "achi"

        ProgramFbf ->
            "fbf"

        ProgramIndividual ->
            "individual"

        ProgramPmtct ->
            "pmtct"

        ProgramSorwathe ->
            "sorwathe"

        ProgramUnknown ->
            "unknown"


encodePeriodsAs : String -> Periods -> ( String, Value )
encodePeriodsAs fieldName periods =
    ( fieldName, object <| encodePeriods periods )


encodePeriods : Periods -> List ( String, Value )
encodePeriods periods =
    [ ( "last_year", int periods.lastYear )
    , ( "this_year", int periods.thisYear )
    ]


encodeVillagesWithResidents : Dict VillageId (List PersonIdentifier) -> ( String, Value )
encodeVillagesWithResidents dict =
    let
        value =
            Dict.toList dict
                |> List.map
                    (\( villageId, idsList ) ->
                        ( fromEntityUuid villageId, list int idsList )
                    )
                |> object
    in
    ( "villages_with_residents", value )


encodePrenatalData : List PrenatalDataItem -> ( String, Value )
encodePrenatalData itemsList =
    ( "prenatal_data", list (encodePrenatalDataItem >> object) itemsList )


encodePrenatalDataItem : PrenatalDataItem -> List ( String, Value )
encodePrenatalDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "expected_date_concluded", maybe encodeYYYYMMDD item.expectedDateConcluded )
    , ( "date_concluded", maybe encodeYYYYMMDD item.dateConcluded )
    , ( "outcome", maybe encodeIndividualEncounterParticipantOutcome item.outcome )
    , ( "delivery_location", maybe encodeDeliveryLocation item.deliveryLocation )
    ]
