module Backend.Endpoints exposing (SessionParams, clinicEndpoint, encodeSessionParams, healthCenterEndpoint, nurseEndpoint, offlineSessionEndpoint, sessionEndpoint, swEndpoint, syncDataEndpoint, trainingSessionsEndpoint)

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Decoder exposing (decodeHealthCenter)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (TrainingSessionRequest)
import Backend.Nurse.Decoder exposing (decodeNurse)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Session.Decoder exposing (decodeOfflineSession, decodeSession, decodeTrainingSessionRequest)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId, encodeSession, encodeTrainingSessionRequest)
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..), OfflineSession, Session)
import Backend.SyncData.Decoder exposing (decodeSyncData)
import Backend.SyncData.Encoder exposing (encodeSyncData)
import Backend.SyncData.Model exposing (SyncData)
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, succeed)
import Json.Encode exposing (Value, object)
import Maybe.Extra exposing (toList)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)


{-| Construct an endpoint that talks to our local service worker in terms of UUIDs.
-}
swEndpoint : String -> Decoder value -> ReadOnlyEndPoint Error (EntityUuid a) value p
swEndpoint path decodeValue =
    let
        decodeKey =
            Json.Decode.map toEntityUuid (field "uuid" Json.Decode.string)
    in
    endpoint path decodeKey decodeValue drupalBackend
        |> withKeyEncoder fromEntityUuid


healthCenterEndpoint : ReadOnlyEndPoint Error HealthCenterId HealthCenter ()
healthCenterEndpoint =
    swEndpoint "nodes/health_center" decodeHealthCenter


syncDataEndpoint : ReadWriteEndPoint Error HealthCenterId SyncData SyncData ()
syncDataEndpoint =
    swEndpoint "nodes/syncmetadata" decodeSyncData
        |> withValueEncoder encodeSyncData


clinicEndpoint : ReadWriteEndPoint Error ClinicId Clinic Clinic ()
clinicEndpoint =
    swEndpoint "nodes/clinic" decodeClinic
        |> withValueEncoder (object << encodeClinic)


nurseEndpoint : ReadOnlyEndPoint Error NurseId Nurse NurseParams
nurseEndpoint =
    swEndpoint "nodes/nurse" decodeNurse
        |> withParamsEncoder encodeNurseParams


type alias NurseParams =
    { pinCode : Maybe String
    }


encodeNurseParams : NurseParams -> List ( String, String )
encodeNurseParams params =
    params.pinCode
        |> Maybe.map (\code -> ( "pin_code", code ))
        |> Maybe.Extra.toList


{-| Type-safe params ... how nice!
-}
type alias SessionParams =
    { openAfter : Maybe NominalDate
    }


encodeSessionParams : SessionParams -> List ( String, String )
encodeSessionParams params =
    params.openAfter
        |> Maybe.map (\open -> ( "open_after", Gizra.NominalDate.formatYYYYMMDD open ))
        |> Maybe.Extra.toList


sessionEndpoint : ReadWriteEndPoint Error SessionId Session Session SessionParams
sessionEndpoint =
    swEndpoint "nodes/session" decodeSession
        |> withValueEncoder (object << encodeSession)
        |> withParamsEncoder encodeSessionParams


trainingSessionsEndpoint : ReadWriteEndPoint Error () TrainingSessionRequest TrainingSessionRequest ()
trainingSessionsEndpoint =
    -- This one is a little different because we're not expecting a key. So, we
    -- just decode the key successfully as `()`. We can't use `drupalEndpont`
    -- directly, because it assumes the key is some kind of `EntityId` (which
    -- is normally convenient). This could change in future if the backend
    -- were to queue the request and give it an ID, instead of executing it
    -- immediately.
    endpoint "api/training_session_actions" (succeed ()) decodeTrainingSessionRequest drupalBackend
        |> withValueEncoder encodeTrainingSessionRequest


offlineSessionEndpoint : ReadWriteEndPoint Error SessionId OfflineSession OfflineSession ()
offlineSessionEndpoint =
    swEndpoint "nodes/offline_session" decodeOfflineSession
        |> withValueEncoder (object << encodeOfflineSession)
