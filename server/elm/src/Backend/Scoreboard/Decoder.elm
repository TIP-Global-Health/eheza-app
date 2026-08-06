module Backend.Scoreboard.Decoder exposing (decodeScoreboardData, decodeSyncResponse)

import AssocList as Dict
import Backend.Components.Decoder exposing (decodeReportParams, decodeSelectedEntity)
import Backend.Decoder exposing (decodeSite)
import Backend.Scoreboard.Model exposing (ANCNewbornData, CriterionBySeverities, InfrastructureEnvironmentWashData, NCDAData, NutritionBehaviorData, NutritionCriterionsData, PatientData, RawVaccinationData, ScoreboardData, SyncResponse, TargetedInterventionsData, UniversalInterventionData, VaccinationProgressDict, VaccineType(..), emptyANCNewbornData, emptyInfrastructureEnvironmentWashData, emptyNCDAData, emptyNutritionBehaviorData, emptyNutritionCriterionsData, emptyTargetedInterventionsData, emptyUniversalInterventionData)
import Backend.Scoreboard.Utils exposing (generateVaccinationProgressForVaccine)
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, bool, field, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Maybe.Extra exposing (isNothing)


decodeScoreboardData : Decoder ScoreboardData
decodeScoreboardData =
    succeed ScoreboardData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "params" decodeReportParams
        |> hardcoded []
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
        |> required "created" decodeYYYYMMDD
        |> required "birth_date" decodeYYYYMMDD
        |> required "edd_date" decodeYYYYMMDD
        |> optional "low_birth_weight" (maybe bool) Nothing
        |> optional "nutrition" decodeNutritionCriterionsData emptyNutritionCriterionsData
        |> optional "ncda" decodeNCDAData emptyNCDAData


decodeNutritionCriterionsData : Decoder NutritionCriterionsData
decodeNutritionCriterionsData =
    succeed NutritionCriterionsData
        |> required "stunting" decodeCriterionBySeverities
        |> required "underweight" decodeCriterionBySeverities
        |> required "wasting" decodeCriterionBySeverities
        |> required "muac" decodeCriterionBySeverities


decodeCriterionBySeverities : Decoder CriterionBySeverities
decodeCriterionBySeverities =
    succeed CriterionBySeverities
        |> optional "severe" (list decodeYYYYMMDD) []
        |> optional "moderate" (list decodeYYYYMMDD) []
        |> optional "normal" (list decodeYYYYMMDD) []
        |> map sanitizeCriterionBySeverities


{-| Guiding rule is that patient should have only one severity value
during calendar month.
In case there are multuple severities, most severe one needs to be selected.
-}
calendarMonth : NominalDate -> ( Int, Int )
calendarMonth date =
    ( Date.year date, Date.monthNumber date )


sanitizeCriterionBySeverities : CriterionBySeverities -> CriterionBySeverities
sanitizeCriterionBySeverities data =
    let
        -- Keying by the month the measurement was taken in leaves one value
        -- per month, which is the month the scorecard presents it under.
        severeDict =
            List.map (\date -> ( calendarMonth date, date ))
                data.severe
                |> Dict.fromList

        moderateDict =
            List.map (\date -> ( calendarMonth date, date ))
                data.moderate
                |> Dict.fromList

        normalDict =
            List.map (\date -> ( calendarMonth date, date ))
                data.normal
                |> Dict.fromList

        -- Filtering out moderate value, in case severe value was taken
        -- during same month.
        sanitizedModerate =
            Dict.toList moderateDict
                |> List.filterMap
                    (\( months, date ) ->
                        if isNothing <| Dict.get months severeDict then
                            Just date

                        else
                            Nothing
                    )

        -- Filtering out normal value, in case severe or moderate values
        -- wewre taken during same month.
        sanitizedNormal =
            Dict.toList normalDict
                |> List.filterMap
                    (\( months, date ) ->
                        if
                            (isNothing <| Dict.get months severeDict)
                                && (isNothing <| Dict.get months moderateDict)
                        then
                            Just date

                        else
                            Nothing
                    )
    in
    { data
        | severe = Dict.values severeDict
        , moderate = sanitizedModerate
        , normal = sanitizedNormal
    }


decodeNCDAData : Decoder NCDAData
decodeNCDAData =
    succeed NCDAData
        |> optional "pane1" decodeANCNewbornData emptyANCNewbornData
        |> optional "pane2" decodeUniversalInterventionData emptyUniversalInterventionData
        |> optional "pane3" decodeNutritionBehaviorData emptyNutritionBehaviorData
        |> optional "pane4" decodeTargetedInterventionsData emptyTargetedInterventionsData
        |> optional "pane5" decodeInfrastructureEnvironmentWashData emptyInfrastructureEnvironmentWashData


decodeANCNewbornData : Decoder ANCNewbornData
decodeANCNewbornData =
    succeed ANCNewbornData
        |> optional "row1" decodeMonthlyValues []
        |> optional "row2" bool False


decodeUniversalInterventionData : Decoder UniversalInterventionData
decodeUniversalInterventionData =
    succeed UniversalInterventionData
        |> optional "row1" decodeVaccinationProgressDict Dict.empty
        |> optional "row2" decodeMonthlyValues []
        |> optional "row3" decodeMonthlyValues []
        |> optional "row4" decodeMonthlyValues []
        |> optional "row5" decodeMonthlyValues []


decodeVaccinationProgressDict : Decoder VaccinationProgressDict
decodeVaccinationProgressDict =
    succeed RawVaccinationData
        |> required "bcg" decodeUniqueDates
        |> required "opv" decodeUniqueDates
        |> required "dtp" decodeUniqueDates
        |> required "dtp_sa" decodeUniqueDates
        |> required "pcv13" decodeUniqueDates
        |> required "rotarix" decodeUniqueDates
        |> required "ipv" decodeUniqueDates
        |> required "mr" decodeUniqueDates
        |> map rawVaccinationDataToVaccinationProgressDict


decodeUniqueDates : Decoder (EverySet NominalDate)
decodeUniqueDates =
    list decodeYYYYMMDD
        |> map EverySet.fromList


rawVaccinationDataToVaccinationProgressDict : RawVaccinationData -> VaccinationProgressDict
rawVaccinationDataToVaccinationProgressDict data =
    [ ( VaccineBCG, generateVaccinationProgressForVaccine data.bcg )
    , ( VaccineOPV, generateVaccinationProgressForVaccine data.opv )
    , ( VaccineDTP, generateVaccinationProgressForVaccine data.dtp )
    , ( VaccineDTPStandalone, generateVaccinationProgressForVaccine data.dtpStandalone )
    , ( VaccinePCV13, generateVaccinationProgressForVaccine data.pcv13 )
    , ( VaccineRotarix, generateVaccinationProgressForVaccine data.rotarix )
    , ( VaccineIPV, generateVaccinationProgressForVaccine data.ipv )
    , ( VaccineMR, generateVaccinationProgressForVaccine data.mr )
    ]
        |> Dict.fromList


decodeNutritionBehaviorData : Decoder NutritionBehaviorData
decodeNutritionBehaviorData =
    succeed NutritionBehaviorData
        |> optional "row1" bool False
        |> optional "row2" decodeMonthlyValues []
        |> optional "row3" decodeMonthlyValues []
        |> optional "row4" decodeMonthlyValues []


decodeTargetedInterventionsData : Decoder TargetedInterventionsData
decodeTargetedInterventionsData =
    succeed TargetedInterventionsData
        |> optional "row1" decodeMonthlyValues []
        |> optional "row2" decodeMonthlyValues []
        |> optional "row3" decodeMonthlyValues []
        |> optional "row4" decodeMonthlyValues []
        |> optional "row5" decodeMonthlyValues []
        |> optional "row6" decodeMonthlyValues []


decodeInfrastructureEnvironmentWashData : Decoder InfrastructureEnvironmentWashData
decodeInfrastructureEnvironmentWashData =
    succeed InfrastructureEnvironmentWashData
        |> optional "row1" decodeMonthlyValues []
        |> optional "row2" decodeMonthlyValues []
        |> optional "row3" decodeMonthlyValues []
        |> optional "row4" decodeMonthlyValues []
        |> optional "row5" decodeMonthlyValues []


decodeMonthlyValues : Decoder (List NominalDate)
decodeMonthlyValues =
    list decodeYYYYMMDD
        |> map sanitizeSingleValuePerMonth


sanitizeSingleValuePerMonth : List NominalDate -> List NominalDate
sanitizeSingleValuePerMonth dates =
    -- Keying by the month the measurement was taken in leaves one value per
    -- month, which is the month the scorecard presents it under.
    List.map (\date -> ( calendarMonth date, date ))
        dates
        |> Dict.fromList
        |> Dict.values
