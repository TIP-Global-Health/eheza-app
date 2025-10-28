module Backend.Reports.Decoder exposing (decodeReportsData, decodeSyncResponse)

import Backend.Components.Decoder exposing (decodeReportParams, decodeSelectedEntity)
import Backend.Decoder exposing (decodeSite, decodeWithFallback)
import Backend.Reports.Model exposing (..)
import Backend.Reports.Utils exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, at, bool, fail, field, list, map, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required)
import Maybe.Extra


decodeReportsData : Decoder ReportsData
decodeReportsData =
    succeed ReportsData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "params" decodeReportParams
        |> hardcoded []
        |> optionalAt [ "additional", "nutrition_report_data" ] (nullable (list decodeBackendGeneratedNutritionReportTableDate)) Nothing
        |> hardcoded Nothing


decodeSyncResponse : Decoder SyncResponse
decodeSyncResponse =
    field "data"
        (succeed SyncResponse
            |> required "batch" (list decodePatientData)
            |> required "total_remaining" decodeInt
            |> required "last" decodeInt
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
        |> optional "o" (nullable decodePregnancyOutcome) Nothing
        |> optional "ol" (nullable decodeDeliveryLocation) Nothing
        |> required "encounters" (list decodePrenatalEncounterData)


decodeDeliveryLocation : Decoder DeliveryLocation
decodeDeliveryLocation =
    string
        |> andThen
            (\s ->
                deliveryLocationFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized DeliveryLocation" |> fail)
            )


deliveryLocationFromString : String -> Maybe DeliveryLocation
deliveryLocationFromString location =
    case location of
        "f" ->
            Just FacilityDelivery

        "h" ->
            Just HomeDelivery

        _ ->
            Nothing


decodePregnancyOutcome : Decoder PregnancyOutcome
decodePregnancyOutcome =
    string
        |> andThen
            (\s ->
                pregnancyOutcomeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized PregnancyOutcome" |> fail)
            )


pregnancyOutcomeFromString : String -> Maybe PregnancyOutcome
pregnancyOutcomeFromString outcome =
    case outcome of
        "a" ->
            Just OutcomeLiveAtTerm

        "b" ->
            Just OutcomeLivePreTerm

        "c" ->
            Just OutcomeStillAtTerm

        "d" ->
            Just OutcomeStillPreTerm

        "e" ->
            Just OutcomeAbortions

        _ ->
            Nothing


decodePrenatalEncounterData : Decoder PrenatalEncounterData
decodePrenatalEncounterData =
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
                                            prenatalEncounterTypeFromString second

                                        diagnoses =
                                            if String.isEmpty third then
                                                []

                                            else
                                                String.split "," third
                                                    |> List.map prenatalDiagnosisFromMapping
                                                    |> Maybe.Extra.values
                                    in
                                    succeed (PrenatalEncounterData startDate encounterType diagnoses)
                                )
                            |> Maybe.withDefault (fail "Failed to decode PrenatalEncounterData")

                    _ ->
                        fail "Failed to decode PrenatalEncounterData"
            )


prenatalEncounterTypeFromString : String -> PrenatalEncounterType
prenatalEncounterTypeFromString encounterType =
    case encounterType of
        "nurse" ->
            NurseEncounter

        "nurse-postpartum" ->
            NursePostpartumEncounter

        "chw-1" ->
            ChwFirstEncounter

        "chw-2" ->
            ChwSecondEncounter

        "chw-3" ->
            ChwThirdPlusEncounter

        "chw-postpartum" ->
            ChwPostpartumEncounter

        _ ->
            -- Fallback.
            NurseEncounter


prenatalDiagnosisFromMapping : String -> Maybe PrenatalDiagnosis
prenatalDiagnosisFromMapping s =
    case s of
        "a" ->
            Just DiagnosisChronicHypertension

        "b" ->
            Just DiagnosisGestationalHypertension

        "c" ->
            Just DiagnosisModeratePreeclampsia

        "d" ->
            Just DiagnosisSeverePreeclampsia

        "g" ->
            Just DiagnosisEclampsia

        "h" ->
            Just DiagnosisHIV

        "i" ->
            Just DiagnosisHIVDetectableViralLoad

        "j" ->
            Just DiagnosisDiscordantPartnership

        "k" ->
            Just DiagnosisSyphilis

        "l" ->
            Just DiagnosisSyphilisWithComplications

        "m" ->
            Just DiagnosisNeurosyphilis

        "n" ->
            Just DiagnosisHepatitisB

        "o" ->
            Just DiagnosisMalaria

        "p" ->
            Just DiagnosisMalariaWithAnemia

        "q" ->
            Just DiagnosisMalariaWithSevereAnemia

        "r" ->
            Just DiagnosisModerateAnemia

        "s" ->
            Just DiagnosisSevereAnemia

        "t" ->
            Just DiagnosisSevereAnemiaWithComplications

        "u" ->
            Just DiagnosisMiscarriage

        "v" ->
            Just DiagnosisMolarPregnancy

        "w" ->
            Just DiagnosisPlacentaPrevia

        "x" ->
            Just DiagnosisPlacentalAbruption

        "y" ->
            Just DiagnosisUterineRupture

        "z" ->
            Just DiagnosisObstructedLabor

        "a1" ->
            Just DiagnosisPostAbortionSepsis

        "b1" ->
            Just DiagnosisEctopicPregnancy

        "c1" ->
            Just DiagnosisPROM

        "d1" ->
            Just DiagnosisPPROM

        "e1" ->
            Just DiagnosisHyperemesisGravidum

        "f1" ->
            Just DiagnosisSevereVomiting

        "g1" ->
            Just DiagnosisMaternalComplications

        "h1" ->
            Just DiagnosisInfection

        "i1" ->
            Just DiagnosisImminentDelivery

        "j1" ->
            Just DiagnosisLaborAndDelivery

        "k1" ->
            Just DiagnosisHeartburn

        "l1" ->
            Just DiagnosisDeepVeinThrombosis

        "m1" ->
            Just DiagnosisPelvicPainIntense

        "n1" ->
            Just DiagnosisUrinaryTractInfection

        "o1" ->
            Just DiagnosisPyelonephritis

        "p1" ->
            Just DiagnosisCandidiasis

        "q1" ->
            Just DiagnosisGonorrhea

        "r1" ->
            Just DiagnosisTrichomonasOrBacterialVaginosis

        "s1" ->
            Just DiagnosisTuberculosis

        "t1" ->
            Just DiagnosisDiabetes

        "u1" ->
            Just DiagnosisGestationalDiabetes

        "v1" ->
            Just DiagnosisRhesusNegative

        "w1" ->
            Just DiagnosisDepressionNotLikely

        "x1" ->
            Just DiagnosisDepressionPossible

        "y1" ->
            Just DiagnosisDepressionHighlyPossible

        "z1" ->
            Just DiagnosisDepressionProbable

        "a2" ->
            Just DiagnosisSuicideRisk

        "b2" ->
            Just DiagnosisOther

        "c2" ->
            Just DiagnosisPostpartumAbdominalPain

        "d2" ->
            Just DiagnosisPostpartumUrinaryIncontinence

        "e2" ->
            Just DiagnosisPostpartumHeadache

        "f2" ->
            Just DiagnosisPostpartumFatigue

        "g2" ->
            Just DiagnosisPostpartumFever

        "h2" ->
            Just DiagnosisPostpartumPerinealPainOrDischarge

        "i2" ->
            Just DiagnosisPostpartumInfection

        "j2" ->
            Just DiagnosisPostpartumExcessiveBleeding

        "k2" ->
            Just DiagnosisPostpartumEarlyMastitisOrEngorgment

        "l2" ->
            Just DiagnosisPostpartumMastitis

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
        [ stunting, underweight, wasting ] ->
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
