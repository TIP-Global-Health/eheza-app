module Backend.Completion.Decoder exposing (decodeCompletionData)

import AssocList as Dict
import Backend.Completion.Model exposing (..)
import Backend.Completion.Utils exposing (..)
import Backend.Decoder exposing (decodeSite, decodeWithFallback)
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, maybe, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Maybe.Extra exposing (isNothing)


decodeCompletionData : Decoder CompletionData
decodeCompletionData =
    succeed CompletionData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> requiredAt [ "results", "nutrition_individual" ] (list (decodeEncounterData decodeNutritionChildActivities))
        |> requiredAt [ "results", "nutrition_group" ]
            (list (decodeNutritionGroupEncounterData decodeNutritionMotherActivities decodeNutritionChildActivities))


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "global" ->
                        succeed EntityGlobal

                    "health-center" ->
                        succeed EntityHealthCenter

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )


decodeEncounterData : Decoder (List activity) -> Decoder (EncounterData activity)
decodeEncounterData activitiesDecoder =
    succeed EncounterData
        |> required "start_date" decodeYYYYMMDD
        |> required "expected" activitiesDecoder
        |> required "completed" activitiesDecoder
        |> required "taken_by" (nullable (decodeWithFallback TakenByUnknown decodeTakenBy))


decodeNutritionChildActivities : Decoder (List NutritionChildActivity)
decodeNutritionChildActivities =
    string
        |> andThen
            (String.split ","
                >> List.map nutritionChildActivityFromMapping
                >> Maybe.Extra.values
                >> succeed
            )


decodeNutritionGroupEncounterData :
    Decoder (List motherActivity)
    -> Decoder (List childActivity)
    -> Decoder (NutritionGroupEncounterData motherActivity childActivity)
decodeNutritionGroupEncounterData motherActivitiesDecoder childActivitiesDecoder =
    succeed NutritionGroupEncounterData
        |> required "start_date" decodeYYYYMMDD
        |> optional "mother" (nullable (decodeActivitiesCompletionData motherActivitiesDecoder)) Nothing
        |> required "children" (list (decodeActivitiesCompletionData childActivitiesDecoder))
        |> required "taken_by" (nullable (decodeWithFallback TakenByUnknown decodeTakenBy))


decodeActivitiesCompletionData : Decoder (List activity) -> Decoder (ActivitiesCompletionData activity)
decodeActivitiesCompletionData activitiesDecoder =
    succeed ActivitiesCompletionData
        |> required "expected" activitiesDecoder
        |> required "completed" activitiesDecoder


decodeNutritionMotherActivities : Decoder (List NutritionMotherActivity)
decodeNutritionMotherActivities =
    string
        |> andThen
            (String.split ","
                >> List.map nutritionMotherActivityFromMapping
                >> Maybe.Extra.values
                >> succeed
            )


decodeTakenBy : Decoder TakenBy
decodeTakenBy =
    string
        |> andThen
            (\takenBy ->
                takenByFromString takenBy
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| takenBy ++ " is unknown TakenBy type")
            )
