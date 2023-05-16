module Backend.Scoreboard.Decoder exposing (decodeScoreboardData)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, float, int, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Maybe.Extra exposing (isNothing)


decodeScoreboardData : NominalDate -> Decoder ScoreboardData
decodeScoreboardData currentDate =
    succeed ScoreboardData
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list (decodePatientData currentDate))


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "district" ->
                        succeed EntityDistrict

                    "sector" ->
                        succeed EntitySector

                    "cell" ->
                        succeed EntityCell

                    "village" ->
                        succeed EntityVillage

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )


decodePatientData : NominalDate -> Decoder PatientData
decodePatientData currentDate =
    succeed PatientData
        |> required "birth_date" decodeYYYYMMDD
        |> optional "low_birth_weight" (maybe bool) Nothing
        |> optional "nutrition" (decodeNutritionCriterionsData currentDate) emptyNutritionCriterionsData
        |> optional "postpartum_checkups" bool False
        |> optional "iron_during_pregnancy" bool False


decodeNutritionCriterionsData : NominalDate -> Decoder NutritionCriterionsData
decodeNutritionCriterionsData currentDate =
    succeed NutritionCriterionsData
        |> required "stunting" (decodeCriterionBySeverities currentDate)
        |> required "underweight" (decodeCriterionBySeverities currentDate)
        |> required "wasting" (decodeCriterionBySeverities currentDate)
        |> required "muac" (decodeCriterionBySeverities currentDate)


decodeCriterionBySeverities : NominalDate -> Decoder CriterionBySeverities
decodeCriterionBySeverities currentDate =
    succeed CriterionBySeverities
        |> optional "severe" (list decodeYYYYMMDD) []
        |> optional "moderate" (list decodeYYYYMMDD) []
        |> optional "normal" (list decodeYYYYMMDD) []
        |> map (sainitzeCriterionBySeverities currentDate)


{-| Guiding rule is that patient should have only one severity value
during calendar month.
In case there are multuple severities, most severe one needs to be selected.
-}
sainitzeCriterionBySeverities : NominalDate -> CriterionBySeverities -> CriterionBySeverities
sainitzeCriterionBySeverities currentDate data =
    let
        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        severeDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.severe
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        moderateDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.moderate
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        normalDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
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
