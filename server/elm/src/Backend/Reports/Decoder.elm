module Backend.Reports.Decoder exposing (decodeReportsData)

import Backend.Decoder exposing (decodeSite)
import Backend.Reports.Model exposing (..)
import Backend.Reports.Utils exposing (..)
import Date
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, list, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required)


decodeReportsData : Decoder ReportsData
decodeReportsData =
    succeed ReportsData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list decodePatientData)
        |> optionalAt [ "additional", "nutrition_report_data" ] (nullable (list decodeBackendGeneratedNutritionReportTableDate)) Nothing


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "global" ->
                        succeed EntityGlobal

                    "province" ->
                        succeed EntityProvince

                    "district" ->
                        succeed EntityDistrict

                    "sector" ->
                        succeed EntitySector

                    "cell" ->
                        succeed EntityCell

                    "village" ->
                        succeed EntityVillage

                    "health-center" ->
                        succeed EntityHealthCenter

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )


decodePatientData : Decoder PatientData
decodePatientData =
    succeed PatientData
        |> required "id" decodeInt
        |> required "created" decodeYYYYMMDD
        |> required "birth_date" decodeYYYYMMDD
        |> required "gender" (decodeWithFallback Female decodeGender)
        |> optionalAt [ "individual", "acute-illness" ] (nullable (list (list decodeAcuteIllnessEncounterData))) Nothing
        |> optionalAt [ "individual", "antenatal" ] (nullable (list decodePrenatalParticipantData)) Nothing
        |> optionalAt [ "individual", "home-visit" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "well-child" ] (nullable (list (list decodeNutritionEncounterData))) Nothing
        |> optionalAt [ "individual", "child-scoreboard" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "ncd" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "hiv" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "tuberculosis" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "nutrition" ] (nullable (list (list decodeNutritionEncounterData))) Nothing
        |> optionalAt [ "group_nutrition", "pmtct" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "fbf" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "sorwathe" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "chw" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "achi" ] (nullable (list decodeNutritionEncounterData)) Nothing


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                genderFromString gender
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| gender ++ " is not a recognized Gender.")
            )


decodeAcuteIllnessEncounterData : Decoder AcuteIllnessEncounterData
decodeAcuteIllnessEncounterData =
    string
        |> andThen
            (\s ->
                case String.split "|" (String.trim s) of
                    [ first, second, third ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        encounterType =
                                            acuteIllnessEncounterTypeFromString second
                                                |> Maybe.withDefault AcuteIllnessEncounterCHW

                                        diagnosis =
                                            acuteIllnessDiagnosisFromMapping third
                                    in
                                    succeed (AcuteIllnessEncounterData startDate encounterType diagnosis)
                                )
                            |> Maybe.withDefault (fail "Failed to decode AcuteIllnessEncounterData")

                    _ ->
                        fail "Failed to decode AcuteIllnessEncounterData"
            )


acuteIllnessEncounterTypeFromString : String -> Maybe AcuteIllnessEncounterType
acuteIllnessEncounterTypeFromString encounterType =
    case encounterType of
        "nurse-encounter" ->
            Just AcuteIllnessEncounterNurse

        "nurse-encounter-subsequent" ->
            Just AcuteIllnessEncounterNurseSubsequent

        "chw-encounter" ->
            Just AcuteIllnessEncounterCHW

        _ ->
            Nothing


acuteIllnessDiagnosisFromMapping : String -> Maybe AcuteIllnessDiagnosis
acuteIllnessDiagnosisFromMapping mapping =
    case mapping of
        "a" ->
            Just DiagnosisCovid19Suspect

        "b" ->
            Just DiagnosisSevereCovid19

        "c" ->
            Just DiagnosisPneuminialCovid19

        "d" ->
            Just DiagnosisLowRiskCovid19

        "e" ->
            Just DiagnosisMalariaComplicated

        "f" ->
            Just DiagnosisMalariaUncomplicated

        "g" ->
            Just DiagnosisMalariaUncomplicatedAndPregnant

        "h" ->
            Just DiagnosisGastrointestinalInfectionComplicated

        "i" ->
            Just DiagnosisGastrointestinalInfectionUncomplicated

        "j" ->
            Just DiagnosisSimpleColdAndCough

        "k" ->
            Just DiagnosisRespiratoryInfectionComplicated

        "l" ->
            Just DiagnosisRespiratoryInfectionUncomplicated

        "m" ->
            Just DiagnosisFeverOfUnknownOrigin

        "n" ->
            Just DiagnosisUndeterminedMoreEvaluationNeeded

        "o" ->
            Just DiagnosisTuberculosisSuspect

        _ ->
            Nothing


decodePrenatalParticipantData : Decoder PrenatalParticipantData
decodePrenatalParticipantData =
    succeed PrenatalParticipantData
        |> required "created" decodeYYYYMMDD
        |> optional "edd" (nullable decodeYYYYMMDD) Nothing
        |> optional "dc" (nullable decodeYYYYMMDD) Nothing
        |> required "encounters" (list decodePrenatalEncounterData)


decodePrenatalEncounterData : Decoder PrenatalEncounterData
decodePrenatalEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed (PrenatalEncounterData startDate NurseEncounter)
                                )
                            |> Maybe.withDefault (fail "Failed to decode PrenatalEncounterData")

                    [ first, second ] ->
                        Maybe.map2
                            (\startDate encounterType ->
                                succeed (PrenatalEncounterData startDate encounterType)
                            )
                            (Date.fromIsoString first |> Result.toMaybe)
                            (prenatalEncounterTypeFromString second)
                            |> Maybe.withDefault (fail "Failed to decode PrenatalEncounterData")

                    _ ->
                        fail "Failed to decode PrenatalEncounterData"
            )


prenatalEncounterTypeFromString : String -> Maybe PrenatalEncounterType
prenatalEncounterTypeFromString encounterType =
    case encounterType of
        "nurse" ->
            Just NurseEncounter

        "nurse-postpartum" ->
            Just NursePostpartumEncounter

        "chw-1" ->
            Just ChwFirstEncounter

        "chw-2" ->
            Just ChwSecondEncounter

        "chw-3" ->
            Just ChwThirdPlusEncounter

        "chw-postpartum" ->
            Just ChwPostpartumEncounter

        _ ->
            Nothing


decodeNutritionEncounterData : Decoder NutritionEncounterData
decodeNutritionEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed (NutritionEncounterData startDate Nothing)
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    [ first, second ] ->
                        (Date.fromIsoString first |> Result.toMaybe)
                            |> Maybe.map
                                (\startDate ->
                                    succeed (NutritionEncounterData startDate (nutritionDataFromString second))
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    _ ->
                        fail "Failed to decode NutritionEncounterData"
            )


nutritionDataFromString : String -> Maybe NutritionData
nutritionDataFromString s =
    case String.split "," s of
        [ stunting, wasting, underweight ] ->
            Just <| NutritionData (String.toFloat stunting) (String.toFloat wasting) (String.toFloat underweight)

        _ ->
            Nothing


decodeBackendGeneratedNutritionReportTableDate : Decoder BackendGeneratedNutritionReportTableDate
decodeBackendGeneratedNutritionReportTableDate =
    succeed BackendGeneratedNutritionReportTableDate
        |> required "type" decodeNutritionReportTableType
        |> required "period" (list string)
        |> required "stunting_moderate" (list string)
        |> required "stunting_severe" (list string)
        |> required "wasting_moderate" (list string)
        |> required "wasting_severe" (list string)
        |> required "underweight_moderate" (list string)
        |> required "underweight_severe" (list string)


decodeNutritionReportTableType : Decoder NutritionReportTableType
decodeNutritionReportTableType =
    string
        |> andThen
            (\tableType ->
                case tableType of
                    "prevalence-1" ->
                        succeed NutritionTablePrevalanceOneOrMore

                    "prevalence-2" ->
                        succeed NutritionTablePrevalanceTwoOrMore

                    "incidence-month-1" ->
                        succeed NutritionTableIncidenceMonthOneOrMore

                    "incidence-month-2" ->
                        succeed NutritionTableIncidenceMonthTwoOrMore

                    "incidence-quarter-1" ->
                        succeed NutritionTableIncidenceQuarterOneOrMore

                    "incidence-quarter-2" ->
                        succeed NutritionTableIncidenceQuarterTwoOrMore

                    "incidence-year-1" ->
                        succeed NutritionTableIncidenceYearOneOrMore

                    "incidence-year-2" ->
                        succeed NutritionTableIncidenceYearTwoOrMore

                    _ ->
                        fail <| tableType ++ " is unknown NutritionReportTableType type"
            )


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
