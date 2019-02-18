module Backend.Session.Decoder exposing (decodeChildren, decodeMothers, decodeSession, decodeTrainingSessionAction, decodeTrainingSessionRequest)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Child.Model exposing (Child)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeHistoricalMeasurements)
import Backend.Measurement.Model exposing (ChildMeasurementList, ChildMeasurements, Measurement, MotherMeasurementList, MotherMeasurements, emptyMeasurements)
import Backend.Model exposing (TrainingSessionAction(..), TrainingSessionRequest)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Mother.Model exposing (Mother)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Session.Model exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Time.Date


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


decodeMothers : Decoder (EveryDictList MotherId Mother)
decodeMothers =
    EveryDictList.decodeArray2 (field "id" decodeEntityUuid) decodeMother
        |> map (EveryDictList.sortBy .name)


decodeChildren : Decoder (EveryDictList ChildId Child)
decodeChildren =
    EveryDictList.decodeArray2 (field "id" decodeEntityUuid) decodeChild
        |> map (EveryDictList.sortBy .name)
