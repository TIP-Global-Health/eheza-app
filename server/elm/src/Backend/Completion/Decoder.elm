module Backend.Completion.Decoder exposing (decodeCompletionData, decodeSyncResponse)

import Backend.Completion.Model exposing (..)
import Backend.Completion.Utils exposing (..)
import Backend.Components.Decoder exposing (decodeReportParams, decodeSelectedEntity)
import Backend.Decoder exposing (decodeSite, decodeWithFallback)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, field, list, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required, requiredAt)


decodeCompletionData : Decoder CompletionData
decodeCompletionData =
    succeed CompletionData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "params" decodeReportParams
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded []
        |> hardcoded Nothing


decodeSyncResponse : NominalDate -> Decoder SyncResponse
decodeSyncResponse currentDate =
    field "data"
        (succeed SyncResponse
            |> requiredAt [ "batch", "acute_illness" ] (list (decodeEncounterData acuteIllnessActivityFromMapping))
            |> requiredAt [ "batch", "child_scoreboard" ] (list (decodeEncounterData childScoreboardActivityFromMapping))
            |> requiredAt [ "batch", "hiv" ] (list (decodeEncounterData hivActivityFromMapping))
            |> requiredAt [ "batch", "home_visit" ] (list (decodeEncounterData homeVisitActivityFromMapping))
            |> requiredAt [ "batch", "ncd" ] (list (decodeEncounterData ncdActivityFromMapping))
            |> requiredAt [ "batch", "nutrition_individual" ] (list (decodeEncounterData nutritionChildActivityFromMapping))
            |> requiredAt [ "batch", "nutrition_group" ]
                (list
                    (decodeNutritionGroupEncounterData
                        nutritionMotherActivityFromMapping
                        nutritionChildActivityFromMapping
                    )
                )
            |> requiredAt [ "batch", "prenatal" ] (list (decodeEncounterData prenatalActivityFromMapping))
            |> requiredAt [ "batch", "tuberculosis" ] (list (decodeEncounterData tuberculosisActivityFromMapping))
            |> requiredAt [ "batch", "well_child" ] (list decodeWellChildEncounterData)
            |> required "total_remaining" decodeInt
            |> required "last" decodeInt
        )


decodeEncounterData : (String -> Maybe activity) -> Decoder (EncounterData activity)
decodeEncounterData activityFromString =
    succeed EncounterData
        |> required "start_date" decodeYYYYMMDD
        |> optional "taken_by" (nullable (decodeWithFallback TakenByUnknown decodeTakenBy)) Nothing
        |> required "completion" (decodeActivitiesCompletionData activityFromString)


decodeNutritionGroupEncounterData :
    (String -> Maybe motherActivity)
    -> (String -> Maybe childActivity)
    -> Decoder (NutritionGroupEncounterData motherActivity childActivity)
decodeNutritionGroupEncounterData motherActivityFromString childActivityFromString =
    succeed NutritionGroupEncounterData
        |> required "start_date" decodeYYYYMMDD
        |> required "taken_by" (nullable (decodeWithFallback TakenByUnknown decodeTakenBy))
        |> optional "mother" (nullable (decodeActivitiesCompletionData motherActivityFromString)) Nothing
        |> required "children" (decodeActivitiesCompletionDataList childActivityFromString)


decodeWellChildEncounterData : Decoder WellChildEncounterData
decodeWellChildEncounterData =
    succeed WellChildEncounterData
        |> required "start_date" decodeYYYYMMDD
        |> required "encounter_type" decodeWellChildEncounterType
        |> required "completion" (decodeActivitiesCompletionData wellChildActivityFromMapping)


decodeWellChildEncounterType : Decoder WellChildEncounterType
decodeWellChildEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "pediatric-care" ->
                        succeed PediatricCare

                    "pediatric-care-chw" ->
                        succeed PediatricCareChw

                    "newborn-exam" ->
                        succeed NewbornExam

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized WellChildEncounterType"
            )


decodeActivitiesCompletionData : (String -> Maybe activity) -> Decoder (ActivitiesCompletionData activity)
decodeActivitiesCompletionData activityFromString =
    string
        |> andThen
            (\s ->
                activitiesCompletionDataFromString activityFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| s ++ " is unknown ActivitiesCompletionData type")
            )


decodeActivitiesCompletionDataList : (String -> Maybe activity) -> Decoder (List (ActivitiesCompletionData activity))
decodeActivitiesCompletionDataList activityFromString =
    oneOf
        [ string
            |> andThen
                (\s ->
                    String.split "$" s
                        |> List.filterMap (activitiesCompletionDataFromString activityFromString)
                        |> succeed
                )
        , succeed []
        ]


activitiesCompletionDataFromString : (String -> Maybe activity) -> String -> Maybe (ActivitiesCompletionData activity)
activitiesCompletionDataFromString activityFromString s =
    let
        splitActivities =
            String.split "," >> List.filterMap activityFromString
    in
    case String.split "|" (String.trim s) of
        [ expected, completed ] ->
            ActivitiesCompletionData
                (splitActivities expected)
                (splitActivities completed)
                |> Just

        [ expected ] ->
            ActivitiesCompletionData
                (splitActivities expected)
                []
                |> Just

        [] ->
            ActivitiesCompletionData [] []
                |> Just

        _ ->
            Nothing


decodeTakenBy : Decoder TakenBy
decodeTakenBy =
    string
        |> andThen
            (\takenBy ->
                takenByFromString takenBy
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| takenBy ++ " is unknown TakenBy type")
            )
