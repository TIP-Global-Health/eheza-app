module Backend.Reports.Decoder exposing (decodeReportsData)

import AssocList as Dict
import Backend.Decoder exposing (decodeSite)
import Backend.Reports.Model exposing (..)
import Backend.Reports.Utils exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, maybe, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Maybe.Extra exposing (isNothing)


decodeReportsData : Decoder ReportsData
decodeReportsData =
    succeed ReportsData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list decodePatientData)


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
        |> optionalAt [ "individual", "acute-illness" ] (nullable (list decodeAcuteIllnessParticipantData)) Nothing
        |> optionalAt [ "individual", "antenatal" ] (nullable (list (list decodePrenatalEncounterData))) Nothing
        |> optionalAt [ "individual", "home-visit" ] (nullable (list (list decodeHomeVisitEncounterData))) Nothing
        |> optionalAt [ "individual", "well-child" ] (nullable (list (list decodeNutritionEncounterData))) Nothing
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


decodeAcuteIllnessParticipantData : Decoder AcuteIllnessParticipantData
decodeAcuteIllnessParticipantData =
    succeed AcuteIllnessParticipantData
        |> required "created" decodeYYYYMMDD
        |> optional "edd" (nullable decodeYYYYMMDD) Nothing
        |> required "encounters" (list decodeAcuteIllnessEncounterData)


decodeAcuteIllnessEncounterData : Decoder AcuteIllnessEncounterData
decodeAcuteIllnessEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed (AcuteIllnessEncounterData startDate AcuteIllnessEncounterCHW)
                                )
                            |> Maybe.withDefault (fail "Failed to decode AcuteIllnessEncounterData")

                    [ first, second ] ->
                        Maybe.map2
                            (\startDate encounterType ->
                                succeed (AcuteIllnessEncounterData startDate encounterType)
                            )
                            (Date.fromIsoString first |> Result.toMaybe)
                            (acuteIllnessEncounterTypeFromString second)
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


decodeHomeVisitEncounterData : Decoder HomeVisitEncounterData
decodeHomeVisitEncounterData =
    decodeYYYYMMDD


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
