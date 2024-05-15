module Backend.Dashboard.Encoder exposing (encodeDashboardStatsRaw)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessDiagnosis, encodeAcuteIllnessEncounterType)
import Backend.Dashboard.Model exposing (..)
import Backend.EducationSession.Encoder exposing (encodeEducationTopic)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Encoder exposing (encodeDeliveryLocation, encodeIndividualEncounterParticipantOutcome)
import Backend.Measurement.Encoder
    exposing
        ( encodeCall114Sign
        , encodeDangerSign
        , encodeFamilyPlanningSign
        , encodeHCContactSign
        , encodeHCRecommendation
        , encodeIsolationSign
        , encodeMedicalCondition
        , encodeNutritionSign
        , encodeRecommendation114
        , encodeSendToHCSign
        , encodeTestExecutionNote
        , encodeTestResult
        )
import Backend.NCDEncounter.Encoder exposing (encodeNCDDiagnosis)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.NutritionEncounter.Encoder exposing (encodeNutritionEncounterType)
import Backend.Person.Encoder exposing (encodeGender)
import Backend.PrenatalEncounter.Encoder exposing (encodePrenatalDiagnosis, encodePrenatalEncounterType)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.WellChildEncounter.Encoder exposing (encodeEncounterWarning, encodeWellChildEncounterType)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..))
import Dict as LegacyDict
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (fromEntityUuid)
import Utils.Json exposing (encodeEverySet, encodeIfSet)


encodeDashboardStatsRaw : DashboardStatsRaw -> List ( String, Value )
encodeDashboardStatsRaw stats =
    [ encodeCasesManagementData stats.caseManagement
    , encodeChildrenBeneficiariesData stats.childrenBeneficiaries
    , encodeCompletedPrograms stats.completedPrograms
    , encodeFamilyPlanning stats.familyPlanning
    , encodeMissedSessions stats.missedSessions
    , encodeTotalEncountersData stats.totalEncounters
    , encodeAcuteIllnessData stats.acuteIllnessData
    , encodePrenatalData stats.prenatalData
    , encodeNCDData stats.ncdData
    , encodePMTCTData stats.pmtctData
    , encodeSPVData stats.spvData
    , encodeChildScoreboardData stats.childScoreboardData
    , encodeNutritionIndividualData stats.nutritionIndividualData
    , encodeNutritionGroupData stats.nutritionGroupData
    , encodeGroupEducationData stats.groupEducationData
    , encodeVillagesWithResidents stats.villagesWithResidents
    , encodePatientsDetails stats.patientsDetails
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
    , ( "birth_date", encodeYYYYMMDD caseManagement.birthDate )
    , ( "gender", encodeGender caseManagement.gender )
    , ( "nutrition", object <| encodeCaseNutrition caseManagement.nutrition )
    ]


encodeCaseNutrition : CaseNutrition -> List ( String, Value )
encodeCaseNutrition caseNutrition =
    [ ( "stunting", dict String.fromInt encodeNutritionValue (dictToLegacyDict caseNutrition.stunting) )
    , ( "underweight", dict String.fromInt encodeNutritionValue (dictToLegacyDict caseNutrition.underweight) )
    , ( "wasting", dict String.fromInt encodeNutritionValue (dictToLegacyDict caseNutrition.wasting) )
    , ( "muac", dict String.fromInt encodeNutritionValue (dictToLegacyDict caseNutrition.muac) )
    , ( "nutrition_signs", dict String.fromInt encodeNutritionValue (dictToLegacyDict caseNutrition.nutritionSigns) )
    ]


dictToLegacyDict : Dict comparable v -> LegacyDict.Dict comparable v
dictToLegacyDict =
    Dict.toList >> LegacyDict.fromList


encodeNutritionValue : NutritionValue -> Value
encodeNutritionValue value =
    case value.class of
        Neutral ->
            null

        _ ->
            String.toFloat value.value
                |> Maybe.map float
                |> Maybe.withDefault null


encodeChildrenBeneficiariesData : Dict ProgramType (List ChildrenBeneficiariesStats) -> ( String, Value )
encodeChildrenBeneficiariesData dict =
    ( "children_beneficiaries"
    , Dict.toList dict
        |> List.map
            (\( programType, casesList ) ->
                ( programTypeToString programType, list (encodeChildrenBeneficiariesStats >> object) casesList )
            )
        |> object
    )


encodeChildrenBeneficiariesStats : ChildrenBeneficiariesStats -> List ( String, Value )
encodeChildrenBeneficiariesStats stats =
    [ ( "id", int stats.identifier )
    , ( "gender", encodeGender stats.gender )
    , ( "birth_date", encodeYYYYMMDD stats.birthDate )
    , ( "created", encodeYYYYMMDD stats.memberSince )
    , ( "graduation_date", encodeYYYYMMDD stats.graduationDate )
    ]
        ++ encodeIfSet "mother_id" stats.motherIdentifier int


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
    [ ( "id", int stats.identifier )
    , ( "gender", encodeGender stats.gender )
    , ( "birth_date", encodeYYYYMMDD stats.birthDate )
    , ( "expected_date", encodeYYYYMMDD stats.expectedDate )
    ]
        ++ encodeIfSet "mother_id" stats.motherIdentifier int


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

        ProgramChw ->
            "chw"

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


encodeAcuteIllnessData : List AcuteIllnessDataItem -> ( String, Value )
encodeAcuteIllnessData itemsList =
    ( "acute_illness_data", list (encodeAcuteIllnessDataItem >> object) itemsList )


encodeAcuteIllnessDataItem : AcuteIllnessDataItem -> List ( String, Value )
encodeAcuteIllnessDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "birth_date", encodeYYYYMMDD item.birthDate )
    , ( "diagnosis", encodeAcuteIllnessDiagnosis item.diagnosis )
    , ( "date_concluded", maybe encodeYYYYMMDD item.dateConcluded )
    , ( "outcome", maybe encodeIndividualEncounterParticipantOutcome item.outcome )
    , ( "encounters", list encodeAcuteIllnessEncounterDataItem item.encounters )
    ]


encodeAcuteIllnessEncounterDataItem : AcuteIllnessEncounterDataItem -> Value
encodeAcuteIllnessEncounterDataItem item =
    object
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "encounter_type", encodeAcuteIllnessEncounterType item.encounterType )
        , ( "sequence_number", int item.sequenceNumber )
        , ( "age_in_months", int item.ageInMonths )
        , ( "diagnosis", encodeAcuteIllnessDiagnosis item.diagnosis )
        , ( "fever", bool item.feverRecorded )
        , ( "isolation", encodeEverySet encodeIsolationSign item.isolationSigns )
        , ( "send_to_hc", encodeEverySet encodeSendToHCSign item.sendToHCSigns )
        , ( "call_114", encodeEverySet encodeCall114Sign item.call114Signs )
        , ( "recommendation_114", encodeEverySet encodeRecommendation114 item.recommendation114 )
        , ( "contact_hc", encodeEverySet encodeHCContactSign item.hcContactSigns )
        , ( "recommendation_hc", encodeEverySet encodeHCRecommendation item.hcRecommendation )
        ]


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
    , ( "encounters", list encodePrenatalEncounterDataItem item.encounters )
    ]


encodePrenatalEncounterDataItem : PrenatalEncounterDataItem -> Value
encodePrenatalEncounterDataItem item =
    let
        diagnosesWithDefault diagnoses =
            if EverySet.isEmpty diagnoses then
                List.singleton NoPrenatalDiagnosis

            else
                EverySet.toList diagnoses

        muac =
            Maybe.map (\value -> [ ( "muac", float value ) ]) item.muac
                |> Maybe.withDefault []
    in
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "danger_signs", encodeEverySet encodeDangerSign item.dangerSigns )
        , ( "encounter_type", encodePrenatalEncounterType item.encounterType )
        , ( "diagnoses", list encodePrenatalDiagnosis (diagnosesWithDefault item.diagnoses) )
        , ( "send_to_hc", encodeEverySet encodeSendToHCSign item.sendToHCSigns )
        ]
            ++ muac


encodeNCDData : List NCDDataItem -> ( String, Value )
encodeNCDData itemsList =
    ( "ncd_data", list (encodeNCDDataItem >> object) itemsList )


encodeNCDDataItem : NCDDataItem -> List ( String, Value )
encodeNCDDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "birth_date", encodeYYYYMMDD item.birthDate )
    , ( "encounters", list encodeNCDEncounterDataItem item.encounters )
    ]


encodeNCDEncounterDataItem : NCDEncounterDataItem -> Value
encodeNCDEncounterDataItem item =
    let
        diagnosesWithDefault diagnoses =
            if EverySet.isEmpty diagnoses then
                List.singleton NoNCDDiagnosis

            else
                EverySet.toList diagnoses

        hivTestResult =
            Maybe.map (\result -> [ ( "hiv_test_result", encodeTestResult result ) ])
                item.hivTestResult
                |> Maybe.withDefault []

        hivTestExecutionNote =
            Maybe.map (\note -> [ ( "hiv_test_execution_note", encodeTestExecutionNote note ) ])
                item.hivTestExecutionNote
                |> Maybe.withDefault []
    in
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "diagnoses", list encodeNCDDiagnosis (diagnosesWithDefault item.diagnoses) )
        , ( "medical_conditions", encodeEverySet encodeMedicalCondition item.medicalConditions )
        , ( "co_morbidities", encodeEverySet encodeMedicalCondition item.coMorbidities )
        ]
            ++ hivTestResult
            ++ hivTestExecutionNote


encodePMTCTData : List PMTCTDataItem -> ( String, Value )
encodePMTCTData itemsList =
    ( "pmtct_data", list (encodePMTCTDataItem >> object) itemsList )


encodePMTCTDataItem : PMTCTDataItem -> List ( String, Value )
encodePMTCTDataItem item =
    [ ( "id", int item.identifier )
    , ( "start_date", encodeYYYYMMDD item.startDate )
    , ( "end_date", encodeYYYYMMDD item.endDate )
    ]


encodeSPVData : List SPVDataItem -> ( String, Value )
encodeSPVData itemsList =
    ( "spv_data", list (encodeSPVDataItem >> object) itemsList )


encodeSPVDataItem : SPVDataItem -> List ( String, Value )
encodeSPVDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "birth_date", encodeYYYYMMDD item.birthDate )
    , ( "gender", encodeGender item.gender )
    , ( "encounters", list encodeSPVEncounterDataItem item.encounters )
    ]


encodeSPVEncounterDataItem : SPVEncounterDataItem -> Value
encodeSPVEncounterDataItem item =
    let
        warningsWithDefault warnings =
            if EverySet.isEmpty warnings then
                List.singleton NoEncounterWarnings

            else
                EverySet.toList warnings

        zscoreStunting =
            Maybe.map (\value -> [ ( "zscore_stunting", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreUnderweight =
            Maybe.map (\value -> [ ( "zscore_underweight", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreWasting =
            Maybe.map (\value -> [ ( "zscore_wasting", float value ) ])
                item.zscoreWasting
                |> Maybe.withDefault []

        muac =
            Maybe.map (\value -> [ ( "muac", float value ) ])
                item.muac
                |> Maybe.withDefault []
    in
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "encounter_type", encodeWellChildEncounterType item.encounterType )
        , ( "warnings", list encodeEncounterWarning (warningsWithDefault item.warnings) )
        , ( "nutrition_signs", encodeEverySet encodeNutritionSign item.nutritionSigns )
        , ( "well_child_bcg_immunisation", encodeEverySet encodeYYYYMMDD item.bcgImminizationDates )
        , ( "well_child_opv_immunisation", encodeEverySet encodeYYYYMMDD item.opvImminizationDates )
        , ( "well_child_dtp_immunisation", encodeEverySet encodeYYYYMMDD item.dtpImminizationDates )
        , ( "well_child_dtp_sa_immunisation", encodeEverySet encodeYYYYMMDD item.dtpStandaloneImminizationDates )
        , ( "well_child_pcv13_immunisation", encodeEverySet encodeYYYYMMDD item.pcv13ImminizationDates )
        , ( "well_child_rotarix_immunisation", encodeEverySet encodeYYYYMMDD item.rotarixImminizationDates )
        , ( "well_child_ipv_immunisation", encodeEverySet encodeYYYYMMDD item.ipvImminizationDates )
        , ( "well_child_mr_immunisation", encodeEverySet encodeYYYYMMDD item.mrImminizationDates )
        , ( "well_child_hpv_immunisation", encodeEverySet encodeYYYYMMDD item.hpvImminizationDates )
        ]
            ++ zscoreStunting
            ++ zscoreUnderweight
            ++ zscoreWasting
            ++ muac


encodeChildScoreboardData : List ChildScoreboardDataItem -> ( String, Value )
encodeChildScoreboardData itemsList =
    ( "child_scoreboard_data", list (encodeChildScoreboardDataItem >> object) itemsList )


encodeChildScoreboardDataItem : ChildScoreboardDataItem -> List ( String, Value )
encodeChildScoreboardDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "birth_date", encodeYYYYMMDD item.birthDate )
    , ( "gender", encodeGender item.gender )
    , ( "encounters", list encodeChildScoreboardEncounterDataItem item.encounters )
    ]


encodeChildScoreboardEncounterDataItem : ChildScoreboardEncounterDataItem -> Value
encodeChildScoreboardEncounterDataItem item =
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "child_scoreboard_bcg_iz", encodeEverySet encodeYYYYMMDD item.bcgImminizationDates )
        , ( "child_scoreboard_opv_iz", encodeEverySet encodeYYYYMMDD item.opvImminizationDates )
        , ( "child_scoreboard_dtp_iz", encodeEverySet encodeYYYYMMDD item.dtpImminizationDates )
        , ( "child_scoreboard_dtp_sa_iz", encodeEverySet encodeYYYYMMDD item.dtpStandaloneImminizationDates )
        , ( "child_scoreboard_pcv13_iz", encodeEverySet encodeYYYYMMDD item.pcv13ImminizationDates )
        , ( "child_scoreboard_rotarix_iz", encodeEverySet encodeYYYYMMDD item.rotarixImminizationDates )
        , ( "child_scoreboard_ipv_iz", encodeEverySet encodeYYYYMMDD item.ipvImminizationDates )
        , ( "child_scoreboard_mr_iz", encodeEverySet encodeYYYYMMDD item.mrImminizationDates )
        ]


encodeNutritionIndividualData : List NutritionIndividualDataItem -> ( String, Value )
encodeNutritionIndividualData itemsList =
    ( "nutrition_individual_data", list (encodeNutritionIndividualDataItem >> object) itemsList )


encodeNutritionIndividualDataItem : NutritionIndividualDataItem -> List ( String, Value )
encodeNutritionIndividualDataItem item =
    [ ( "id", int item.identifier )
    , ( "created", encodeYYYYMMDD item.created )
    , ( "birth_date", encodeYYYYMMDD item.birthDate )
    , ( "encounters", list encodeNutritionIndividualEncounterDataItem item.encounters )
    ]


encodeNutritionIndividualEncounterDataItem : NutritionIndividualEncounterDataItem -> Value
encodeNutritionIndividualEncounterDataItem item =
    let
        zscoreStunting =
            Maybe.map (\value -> [ ( "zscore_stunting", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreUnderweight =
            Maybe.map (\value -> [ ( "zscore_underweight", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreWasting =
            Maybe.map (\value -> [ ( "zscore_wasting", float value ) ])
                item.zscoreWasting
                |> Maybe.withDefault []

        muac =
            Maybe.map (\value -> [ ( "muac", float value ) ])
                item.muac
                |> Maybe.withDefault []
    in
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "encounter_type", encodeNutritionEncounterType item.encounterType )
        , ( "nutrition_signs", encodeEverySet encodeNutritionSign item.nutritionSigns )
        ]
            ++ zscoreStunting
            ++ zscoreUnderweight
            ++ zscoreWasting
            ++ muac


encodeNutritionGroupData : List NutritionGroupDataItem -> ( String, Value )
encodeNutritionGroupData itemsList =
    ( "nutrition_group_data", list (encodeNutritionGroupDataItem >> object) itemsList )


encodeNutritionGroupDataItem : NutritionGroupDataItem -> List ( String, Value )
encodeNutritionGroupDataItem item =
    [ ( "id", int item.identifier )
    , ( "encounters", list encodeNutritionGroupEncounterDataItem item.encounters )
    ]


encodeNutritionGroupEncounterDataItem : NutritionGroupEncounterDataItem -> Value
encodeNutritionGroupEncounterDataItem item =
    let
        zscoreStunting =
            Maybe.map (\value -> [ ( "zscore_stunting", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreUnderweight =
            Maybe.map (\value -> [ ( "zscore_underweight", float value ) ])
                item.zscoreStunting
                |> Maybe.withDefault []

        zscoreWasting =
            Maybe.map (\value -> [ ( "zscore_wasting", float value ) ])
                item.zscoreWasting
                |> Maybe.withDefault []

        muac =
            Maybe.map (\value -> [ ( "muac", float value ) ])
                item.muac
                |> Maybe.withDefault []
    in
    object <|
        [ ( "start_date", encodeYYYYMMDD item.startDate )
        , ( "nutrition_signs", encodeEverySet encodeNutritionSign item.nutritionSigns )
        ]
            ++ zscoreStunting
            ++ zscoreUnderweight
            ++ zscoreWasting
            ++ muac


encodePatientsDetails : Dict PersonIdentifier PatientDetails -> ( String, Value )
encodePatientsDetails dict =
    let
        value =
            Dict.toList dict
                |> List.map
                    (\( k, v ) ->
                        ( String.fromInt k, encodePatientDetails v )
                    )
                |> object
    in
    ( "patients_details", value )


encodePatientDetails : PatientDetails -> Value
encodePatientDetails details =
    object <|
        [ ( "name", string details.name )
        , ( "gender", encodeGender details.gender )
        ]
            ++ encodeIfSet "phone_number" details.phoneNumber string


encodeGroupEducationData : Dict VillageId (List EducationSessionData) -> ( String, Value )
encodeGroupEducationData dict =
    let
        value =
            Dict.toList dict
                |> List.map
                    (\( villageId, sessionData ) ->
                        ( fromEntityUuid villageId, list encodeEducationSessionData sessionData )
                    )
                |> object
    in
    ( "group_education_data", value )


encodeEducationSessionData : EducationSessionData -> Value
encodeEducationSessionData data =
    object
        [ ( "start_date", encodeYYYYMMDD data.startDate )
        , ( "education_topics", encodeEverySet encodeEducationTopic data.topics )
        , ( "participating_patients", encodeEverySet int data.participants )
        ]
