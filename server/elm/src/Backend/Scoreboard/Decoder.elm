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
        |> optional "stunting_severe" (list decodeYYYYMMDD) []
        |> optional "stunting_moderate" (list decodeYYYYMMDD) []
        |> optional "stunting_normal" (list decodeYYYYMMDD) []
        |> optional "postpartum_checkups" bool False
        |> optional "iron_during_pregnancy" bool False
        |> map (sainitzePatientData currentDate)


{-| Guiding rule is that patient should have only one stunting value
during calendar month.
In case there are multuple values, most severe one needs to be selected.
-}
sainitzePatientData : NominalDate -> PatientData -> PatientData
sainitzePatientData currentDate data =
    let
        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        stuntingSevereDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.stuntingSevere
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        stuntingModerateDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.stuntingModerate
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        stuntingNormalDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.stuntingNormal
                |> Dict.fromList

        -- Filtering out moderate value, in case severe value was taken
        -- during same month.
        sanitizedStuntingModerate =
            Dict.toList stuntingModerateDict
                |> List.filterMap
                    (\( months, date ) ->
                        if isNothing <| Dict.get months stuntingSevereDict then
                            Just date

                        else
                            Nothing
                    )

        -- Filtering out normal value, in case severe or moderate values
        -- wewre taken during same month.
        sanitizedStuntingNormal =
            Dict.toList stuntingNormalDict
                |> List.filterMap
                    (\( months, date ) ->
                        if
                            (isNothing <| Dict.get months stuntingSevereDict)
                                && (isNothing <| Dict.get months stuntingModerateDict)
                        then
                            Just date

                        else
                            Nothing
                    )
    in
    { data
        | stuntingSevere = Dict.values stuntingSevereDict
        , stuntingModerate = sanitizedStuntingModerate
        , stuntingNormal = sanitizedStuntingNormal
    }
