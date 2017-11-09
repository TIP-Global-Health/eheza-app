module Backend.Update exposing (updateBackend)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession, decodeOfflineSession)
import Backend.Session.Model exposing (Session, OfflineSession)
import Config.Model exposing (BackendUrl)
import Restful.Endpoint exposing (EndPoint, toEntityId, fromEntityId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Maybe.Extra exposing (toList)
import RemoteData exposing (RemoteData(..))


clinicEndpoint : EndPoint Error () ClinicId Clinic
clinicEndpoint =
    { path = "api/clinics"
    , tag = toEntityId
    , untag = fromEntityId
    , decoder = decodeClinic
    , error = identity
    , params = always []
    }


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


sessionEndpoint : EndPoint Error SessionParams SessionId Session
sessionEndpoint =
    { path = "api/sessions"
    , tag = toEntityId
    , untag = fromEntityId
    , decoder = decodeSession
    , error = identity
    , params = encodeSessionParams
    }


offlineSessionEndpoint : EndPoint Error () SessionId OfflineSession
offlineSessionEndpoint =
    { path = "api/offline_sessions"
    , tag = toEntityId
    , untag = fromEntityId
    , decoder = decodeOfflineSession
    , error = identity
    , params = always []
    }


updateBackend : BackendUrl -> String -> MsgBackend -> ModelBackend -> ( ModelBackend, Cmd MsgBackend )
updateBackend backendUrl accessToken msg model =
    let
        -- Partially apply the backendUrl and accessToken, just for fun
        selectFromBackend =
            Restful.Endpoint.select backendUrl (Just accessToken)

        getFromBackend =
            Restful.Endpoint.get backendUrl (Just accessToken)
    in
        case msg of
            FetchClinics ->
                -- Ultimately, it would be nice to preserve any existing value of clnics
                -- if we're reloading ... will need an `UpdateableWebData` for that.
                ( { model | clinics = Loading }
                , selectFromBackend clinicEndpoint () <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedClinics)
                )

            HandleFetchedClinics clinics ->
                ( { model | clinics = clinics }
                , Cmd.none
                )

            FetchFutureSessions date ->
                ( { model | futureSessions = Loading }
                , selectFromBackend sessionEndpoint (SessionParams (Just date)) <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedSessions date)
                )

            HandleFetchedSessions date result ->
                -- We remember the date as well as the result, so that we can
                -- know whether we need to reload (i.e. when the date changes,
                -- due to the passage of time)
                ( { model | futureSessions = RemoteData.map (\sessions -> ( date, sessions )) result }
                , Cmd.none
                )

            FetchOfflineSessionFromBackend sessionId ->
                Debug.crash "redo"

            {-
               ( { model | offlineSession = Loading }
               , getFromBackend offlineSessionEndpoint sessionId <|
                   (RemoteData.fromResult >> HandleFetchedOfflineSessionFromBackend)
               )
            -}
            HandleFetchedOfflineSessionFromBackend data ->
                Debug.crash "redo"
