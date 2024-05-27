module Backend.Reports.Decoder exposing (decodeReportsData)

import AssocList as Dict
import Backend.Decoder exposing (decodeSite)
import Backend.Reports.Model exposing (..)
import Backend.Reports.Utils exposing (..)
import Date
import EverySet exposing (EverySet)
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
        |> required "created" decodeYYYYMMDD
        |> required "birth_date" decodeYYYYMMDD
        |> required "gender" (decodeWithFallback Female decodeGender)
        |> optionalAt [ "individual", "acute-illness" ] (nullable (list (list decodeAcuteIllnessEncounterData))) Nothing
        |> optionalAt [ "individual", "antenatal" ] (nullable (list (list decodePrenatalEncounterData))) Nothing
        |> optionalAt [ "individual", "home-visit" ] (nullable (list decodeEncountersData)) Nothing
        |> optionalAt [ "individual", "well-chil" ] (nullable (list decodeEncountersData)) Nothing
        |> optionalAt [ "individual", "nutrition" ] (nullable (list decodeEncountersData)) Nothing
        |> optionalAt [ "group_nutrition", "pmtct" ] (nullable decodeEncountersData) Nothing
        |> optionalAt [ "group_nutrition", "fbf" ] (nullable decodeEncountersData) Nothing
        |> optionalAt [ "group_nutrition", "sorwathe" ] (nullable decodeEncountersData) Nothing
        |> optionalAt [ "group_nutrition", "chw" ] (nullable decodeEncountersData) Nothing
        |> optionalAt [ "group_nutrition", "achi" ] (nullable decodeEncountersData) Nothing


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                genderFromString gender
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| gender ++ " is not a recognized Gender.")
            )


decodeEncountersData : Decoder EncountersData
decodeEncountersData =
    list decodeYYYYMMDD


decodeAcuteIllnessEncounterData : Decoder AcuteIllnessEncounterData
decodeAcuteIllnessEncounterData =
    succeed AcuteIllnessEncounterData
        |> required "start_date" decodeYYYYMMDD
        |> required "encounter_type" (decodeWithFallback AcuteIllnessEncounterCHW decodeAcuteIllnessEncounterType)


decodeAcuteIllnessEncounterType : Decoder AcuteIllnessEncounterType
decodeAcuteIllnessEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse-encounter" ->
                        succeed AcuteIllnessEncounterNurse

                    "nurse-encounter-subsequent" ->
                        succeed AcuteIllnessEncounterNurseSubsequent

                    "chw-encounter" ->
                        succeed AcuteIllnessEncounterCHW

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized AcuteIllnessEncounterType"
            )


decodePrenatalEncounterData : Decoder PrenatalEncounterData
decodePrenatalEncounterData =
    succeed PrenatalEncounterData
        |> required "start_date" decodeYYYYMMDD
        |> required "encounter_type" (decodeWithFallback NurseEncounter decodePrenatalEncounterType)


decodePrenatalEncounterType : Decoder PrenatalEncounterType
decodePrenatalEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NurseEncounter

                    "nurse-postpartum" ->
                        succeed NursePostpartumEncounter

                    "chw-1" ->
                        succeed ChwFirstEncounter

                    "chw-2" ->
                        succeed ChwSecondEncounter

                    "chw-3" ->
                        succeed ChwThirdPlusEncounter

                    "chw-postpartum" ->
                        succeed ChwPostpartumEncounter

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized PrenatalEncounterType"
            )


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
