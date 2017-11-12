port module Backend.Update exposing (updateBackend, updateCache)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession, decodeOfflineSession)
import Backend.Session.Encoder exposing (encodeOfflineSession)
import Backend.Session.Model exposing (Session, OfflineSession)
import Config.Model exposing (BackendUrl)
import Restful.Endpoint exposing (EndPoint, toEntityId, fromEntityId, encodeEntityId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Json.Encode exposing (Value, object)
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


updateBackend : BackendUrl -> String -> MsgBackend -> ModelBackend -> ( ModelBackend, Cmd MsgBackend, List MsgCached )
updateBackend backendUrl accessToken msg model =
    let
        -- Partially apply the backendUrl and accessToken, just for fun
        selectFromBackend =
            Restful.Endpoint.select backendUrl (Just accessToken)

        getFromBackend404 =
            Restful.Endpoint.get404 backendUrl (Just accessToken)
    in
        case msg of
            FetchClinics ->
                -- Ultimately, it would be nice to preserve any existing value of clnics
                -- if we're reloading ... will need an `UpdateableWebData` for that.
                ( { model | clinics = Loading }
                , selectFromBackend clinicEndpoint () <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedClinics)
                , []
                )

            HandleFetchedClinics clinics ->
                ( { model | clinics = clinics }
                , Cmd.none
                , []
                )

            FetchFutureSessions date ->
                ( { model | futureSessions = Loading }
                , selectFromBackend sessionEndpoint (SessionParams (Just date)) <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedSessions date)
                , []
                )

            HandleFetchedSessions date result ->
                -- We remember the date as well as the result, so that we can
                -- know whether we need to reload (i.e. when the date changes,
                -- due to the passage of time)
                ( { model | futureSessions = RemoteData.map (\sessions -> ( date, sessions )) result }
                , Cmd.none
                , []
                )

            FetchOfflineSessionFromBackend sessionId ->
                ( { model | offlineSessionRequest = Loading }
                , getFromBackend404 offlineSessionEndpoint sessionId HandleFetchedOfflineSessionFromBackend
                , []
                )

            HandleFetchedOfflineSessionFromBackend result ->
                case result of
                    Err error ->
                        ( { model | offlineSessionRequest = RemoteData.fromResult (Result.map Tuple.first result) }
                        , Cmd.none
                        , []
                        )

                    Ok ( sessionId, session ) ->
                        -- We immediately kick off a save into the cache
                        ( { model | offlineSessionRequest = Success sessionId }
                        , Cmd.none
                        , [ CacheOfflineSession sessionId session ]
                        )

            ResetOfflineSessionRequest ->
                ( { model | offlineSessionRequest = NotAsked }
                , Cmd.none
                , []
                )


updateCache : MsgCached -> ModelCached -> ( ModelCached, Cmd MsgCached )
updateCache msg model =
    case msg of
        CacheOfflineSession sessionId session ->
            -- We mark that we have the offline session, but note that the
            -- request to update it in the cache is pending.
            ( { model
                | offlineSession =
                    { value = Success (Just ( sessionId, session ))
                    , update = Loading
                    }
              }
            , object
                [ ( "sessionId", encodeEntityId sessionId )
                , ( "session", encodeOfflineSession session )
                ]
                |> Json.Encode.encode 0
                |> cacheOfflineSession
            )

        CacheOfflineSessionResult result ->
            -- TODO: Actually do something with the result.
            let
                offlineSession =
                    model.offlineSession
            in
                ( { model | offlineSession = { offlineSession | update = Success () } }
                , Cmd.none
                )

        FetchOfflineSessionFromCache ->
            Debug.crash "implement"


{-| Cache an offline session. For now, we've just got one slot ... of course,
we can do something more sophisticated when necessary. (We'd need to parameterize
each of the ports via a SessionId.)

The string is some JSON-encoded data ... so that the Javascript side of this
just needs to stuff it somewhere.

TODO: It might be nice to have a module that encapsulates some cache-related
functionality. You could imagine just two ports ... one outgoing and one
incoming ... with some JSON-encodings that specify the operation and data.

-}
port cacheOfflineSession : String -> Cmd msg


{-| We want to get a possible error code back from `cacheOfflineSession`, so
we need an incoming port.
-}
port cacheOfflineSessionResult : (Value -> msg) -> Sub msg


{-| Fetch our offline session. Again, just one slot.
-}
port fetchOfflineSession : () -> Cmd msg


{-| Delete our offline session.
-}
port deleteOfflineSession : () -> Cmd msg


{-| Receive an offline session from the cache.

The string is whatever was provided to `cacheOfflineSession`.

-}
port handleOfflineSession : (String -> msg) -> Sub msg
