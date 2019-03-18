module Backend.Session.Decoder exposing (decodeSession, decodeTrainingSessionAction, decodeTrainingSessionRequest)

import Backend.Model exposing (TrainingSessionAction(..), TrainingSessionRequest)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeTrainingSessionRequest : Decoder TrainingSessionRequest
decodeTrainingSessionRequest =
    succeed TrainingSessionRequest
        |> required "action" decodeTrainingSessionAction


decodeTrainingSessionAction : Decoder TrainingSessionAction
decodeTrainingSessionAction =
    string
        |> andThen
            (\action ->
                case action of
                    "create_all" ->
                        succeed CreateAll

                    "delete_all" ->
                        succeed DeleteAll

                    _ ->
                        fail <| action ++ " is not a recognized TrainingSessionAction"
            )


{-| Decodes the JSON sent by /api/sessions
-}
decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> custom
            (oneOf
                -- Work with "full_view" true or false, or with the
                -- structure we encode for the cache.
                [ field "clinic" decodeEntityUuid
                , field "clinic_id" decodeEntityUuid
                , at [ "clinic", "id" ] decodeEntityUuid
                ]
            )
        |> optional "closed" bool False
        |> optional "training" bool False
