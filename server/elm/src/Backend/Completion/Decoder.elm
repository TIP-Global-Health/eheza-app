module Backend.Completion.Decoder exposing (decodeCompletionData)

import AssocList as Dict
import Backend.Completion.Model exposing (..)
import Backend.Decoder exposing (decodeSite)
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
