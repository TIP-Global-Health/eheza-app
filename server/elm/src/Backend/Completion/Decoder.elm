module Backend.Completion.Decoder exposing (decodeCompletionData)

import AssocList as Dict
import Backend.Completion.Model exposing (..)
import Backend.Decoder exposing (decodeSite, decodeWithFallback)
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, maybe, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Maybe.Extra exposing (isNothing)


decodeCompletionData : Decoder CompletionData
decodeCompletionData =
    succeed CompletionData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list (decodeEncounterData decodeNutritionActivities))


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


decodeNutritionActivities : Decoder (List NutritionActivity)
decodeNutritionActivities =
    string
        |> andThen
            (String.split ","
                >> List.map nutritionActivityFromMapping
                >> Maybe.Extra.values
                >> succeed
            )


nutritionActivityFromMapping : String -> Maybe NutritionActivity
nutritionActivityFromMapping mapped =
    case mapped of
        "a" ->
            Just NutritionHeight

        "b" ->
            Just NutritionNutrition

        "c" ->
            Just NutritionPhoto

        "d" ->
            Just NutritionWeight

        "e" ->
            Just NutritionMUAC

        "f" ->
            Just NutritionContributingFactors

        "g" ->
            Just NutritionFollowUp

        "h" ->
            Just NutritionHealthEducation

        "i" ->
            Just NutritionSendToHC

        "j" ->
            Just NutritionNCDA

        _ ->
            Nothing


decodeTakenBy : Decoder TakenBy
decodeTakenBy =
    string
        |> andThen
            (\takenBy ->
                case takenBy of
                    "nurse" ->
                        succeed TakenByNurse

                    "chw" ->
                        succeed TakenByCHW

                    "unknown" ->
                        succeed TakenByUnknown

                    _ ->
                        fail <| takenBy ++ " is unknown TakenBy type"
            )
