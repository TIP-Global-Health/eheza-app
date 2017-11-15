port module Backend.Update exposing (updateBackend, updateCache, subscriptions, fetchEditableSession)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Activity.Utils exposing (setCheckedIn)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeMeasurementEdits)
import Backend.Measurement.Encoder exposing (encodeMeasurementEdits)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession, decodeOfflineSession, decodeOfflineSessionWithId)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId)
import Backend.Session.Model exposing (Session, OfflineSession, EditableSession, MsgEditableSession(..))
import Backend.Session.Utils exposing (makeEditableSession)
import Config.Model exposing (BackendUrl)
import Restful.Endpoint exposing (EndPoint, toEntityId, fromEntityId, encodeEntityId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Json.Decode
import Json.Encode exposing (Value, object)
import Maybe.Extra exposing (toList)
import RemoteData exposing (RemoteData(..))
import Update.Extra exposing (sequence)


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
                        , [ SetEditableSession sessionId (makeEditableSession session) ]
                        )

            ResetOfflineSessionRequest ->
                ( { model | offlineSessionRequest = NotAsked }
                , Cmd.none
                , []
                )


updateCache : MsgCached -> ModelCached -> ( ModelCached, Cmd MsgCached )
updateCache msg model =
    case msg of
        CacheEditableSession ->
            withEditableSession ( model, Cmd.none )
                (\sessionId session ->
                    let
                        json =
                            ( encodeOfflineSessionWithId sessionId session.offlineSession
                                |> Json.Encode.encode 0
                            , encodeMeasurementEdits session.edits
                                |> Json.Encode.encode 0
                            )
                    in
                        ( { model | editableSession = Success <| Just ( sessionId, { session | update = Loading } ) }
                        , cacheEditableSession json
                        )
                )
                model

        CacheEditableSessionResult result ->
            -- TODO: Actually do something with the result. For now, we just mark Success.
            withEditableSession ( model, Cmd.none )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Success () } ) }
                    , Cmd.none
                    )
                )
                model

        CacheEdits ->
            withEditableSession ( model, Cmd.none )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Loading } ) }
                    , encodeMeasurementEdits session.edits
                        |> Json.Encode.encode 0
                        |> cacheEdits
                    )
                )
                model

        CacheEditsResult result ->
            -- TODO: Actually consult the result ...
            withEditableSession ( model, Cmd.none )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Success () } ) }
                    , Cmd.none
                    )
                )
                model

        DeleteEditableSession ->
            ( { model | editableSession = Success Nothing }
            , deleteEditableSession ()
            )

        FetchEditableSessionFromCache ->
            ( { model | editableSession = Loading }
            , fetchEditableSession ()
            )

        HandleEditableSession ( offlineSessionJson, editsJson ) ->
            let
                decodedOfflineSession =
                    Json.Decode.decodeString decodeOfflineSessionWithId offlineSessionJson

                decodedEdits =
                    Json.Decode.decodeString decodeMeasurementEdits editsJson

                decodedEditableSession =
                    Result.map2
                        (\( sessionId, offlineSession ) edits ->
                            makeEditableSession offlineSession
                                |> (\session ->
                                        ( sessionId
                                        , { session | edits = edits }
                                        )
                                   )
                        )
                        decodedOfflineSession
                        decodedEdits
            in
                case decodedEditableSession of
                    Ok result ->
                        ( { model | editableSession = Success <| Just result }
                        , Cmd.none
                        )

                    Err err ->
                        -- TODO: Actually think about the error. for now, we just say
                        -- we don't have one.
                        let
                            _ =
                                Debug.log "error fetching session from cache" err
                        in
                            ( { model | editableSession = Success Nothing }
                            , Cmd.none
                            )

        MsgEditableSession subMsg ->
            case subMsg of
                SetCheckedIn motherId checkedIn ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, setCheckedIn checkedIn motherId session ) }
                            , Cmd.none
                            )
                                |> sequence updateCache [ CacheEdits ]
                        )
                        model

        SetEditableSession sessionId session ->
            ( { model | editableSession = Success <| Just ( sessionId, session ) }
            , Cmd.none
            )
                |> sequence updateCache [ CacheEditableSession ]


{-| Our editable session is inside a `RemoteData` and a `Maybe`, so it's
convenient to be able to unwrap it without too much verbosity.

Given the model, we apply your function to the editable session. If we don't
have an editable session (i.e. NotAsked or Nothing), we use the default instead
(your first parameter).

TODO: The fact we need this suggests that perhaps the types could be better
arranged. Or, perhaps this is the best we can do.

-}
withEditableSession : a -> (SessionId -> EditableSession -> a) -> ModelCached -> a
withEditableSession default func model =
    model.editableSession
        |> RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.map (uncurry func)
        |> Maybe.withDefault default


{-| Subscribe to the answers to our cache requests.
-}
subscriptions : Sub MsgCached
subscriptions =
    Sub.batch
        [ cacheEditableSessionResult CacheEditableSessionResult
        , cacheEditsResult CacheEditsResult
        , handleEditableSession HandleEditableSession
        ]


{-| Cache an offline session. For now, we've just got one slot ... of course,
we can do something more sophisticated when necessary. (We'd need to parameterize
each of the ports via a SessionId.)

The first string is the offlineSession part, and the second string the edits.
We cache them separately, because we basically treat the offlineSession as
immutable, so we don't have to save it over and over.

The string is some JSON-encoded data ... so that the Javascript side of this
just needs to stuff it somewhere.

TODO: It might be nice to have a module that encapsulates some cache-related
functionality. You could imagine just two ports ... one outgoing and one
incoming ... with some JSON-encodings that specify the operation and data.
We could, for instance, cut down on the number of ports that way ...

-}
port cacheEditableSession : ( String, String ) -> Cmd msg


{-| We want to get a possible error code back from `cacheEditableSession`, so
we need an incoming port.

TODO: Actually define a type to convert the Value to, and actually catch
some errors.

-}
port cacheEditableSessionResult : (Value -> msg) -> Sub msg


{-| Like `cacheEditableSession`, but only caches the edits. This assumes that
you've got the appropriate editable session cached already (we treat it as
immutable).
-}
port cacheEdits : String -> Cmd msg


port cacheEditsResult : (Value -> msg) -> Sub msg


{-| Fetch an editable session. Again, just one slot.
-}
port fetchEditableSession : () -> Cmd msg


{-| Delete our editable session.
-}
port deleteEditableSession : () -> Cmd msg


{-| Receive an editable session from the cache.

The strings are whatever was provided to `cacheEdtiableSession`.

-}
port handleEditableSession : (( String, String ) -> msg) -> Sub msg
