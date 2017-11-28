port module Backend.Update exposing (updateBackend, updateCache, subscriptions, fetchEditableSession)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Activity.Utils exposing (setCheckedIn)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Encoder exposing (encodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeMeasurementEdits)
import Backend.Measurement.Encoder exposing (encodeMeasurementEdits)
import Backend.Measurement.Model exposing (Edit(..))
import Backend.Measurement.Utils exposing (backendValue, mapMeasurementData)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession, decodeOfflineSession)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId, encodeSession)
import Backend.Session.Model exposing (Session, OfflineSession, EditableSession, MsgEditableSession(..))
import Backend.Session.Utils exposing (makeEditableSession, mapChildEdits, mapMotherEdits, getChildMeasurementData, getMotherMeasurementData)
import Backend.Utils exposing (withEditableSession)
import Config.Model exposing (BackendUrl)
import Restful.Endpoint exposing (EndPoint, toEntityId, fromEntityId, encodeEntityId, decodeEntityId)
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Http exposing (Error)
import Json.Decode
import Json.Encode exposing (Value, object)
import Maybe.Extra exposing (toList)
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Update.Extra exposing (sequence)
import Utils.WebData exposing (resetError)


clinicEndpoint : EndPoint Error () ClinicId Clinic
clinicEndpoint =
    { path = "api/clinics"
    , tag = toEntityId
    , untag = fromEntityId
    , decoder = decodeClinic
    , encoder = object << encodeClinic
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
    , encoder = encodeSession
    , error = identity
    , params = encodeSessionParams
    }


offlineSessionEndpoint : EndPoint Error () SessionId OfflineSession
offlineSessionEndpoint =
    { path = "api/offline_sessions"
    , tag = toEntityId
    , untag = fromEntityId
    , decoder = decodeOfflineSession
    , encoder = object << encodeOfflineSession
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

        patchBackend =
            Restful.Endpoint.patch_ backendUrl (Just accessToken)

        resetErrorsIfSucceeded data =
            sequenceExtra (updateBackend backendUrl accessToken) <|
                case data of
                    Success _ ->
                        [ ResetErrors ]

                    _ ->
                        []

        resetErrorsIfOk result =
            sequenceExtra (updateBackend backendUrl accessToken) <|
                case result of
                    Ok _ ->
                        [ ResetErrors ]

                    Err _ ->
                        []
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
                    |> resetErrorsIfSucceeded clinics

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
                    |> resetErrorsIfSucceeded result

            FetchOfflineSessionFromBackend sessionId ->
                ( { model | offlineSessionRequest = Loading }
                , getFromBackend404 offlineSessionEndpoint sessionId HandleFetchedOfflineSessionFromBackend
                , []
                )

            HandleFetchedOfflineSessionFromBackend result ->
                resetErrorsIfOk result <|
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

            ResetErrors ->
                -- Reset some error conditions to `NotAsked`, so that they will
                -- be automatically retried if needed.
                ( { model
                    | clinics = resetError model.clinics
                    , futureSessions = resetError model.futureSessions
                  }
                , Cmd.none
                , []
                )

            ResetOfflineSessionRequest ->
                ( { model | offlineSessionRequest = NotAsked }
                , Cmd.none
                , []
                )

            UploadEdits sessionId edits ->
                ( { model | uploadEditsRequest = Loading }
                , patchBackend offlineSessionEndpoint sessionId (encodeMeasurementEdits edits) (HandleUploadedEdits sessionId)
                , []
                )

            HandleUploadedEdits sessionId result ->
                resetErrorsIfOk result <|
                    case result of
                        Err error ->
                            ( { model | uploadEditsRequest = RemoteData.fromResult (Result.map (always sessionId) result) }
                            , Cmd.none
                            , []
                            )

                        Ok _ ->
                            -- Record success, and delete our locally cached session.
                            -- We also invalidate our `futureSessions`, which will indirectly make us fetch them again.
                            ( { model
                                | uploadEditsRequest = Success sessionId
                                , futureSessions = NotAsked
                              }
                            , Cmd.none
                            , [ DeleteEditableSession ]
                            )

            ResetUploadEditsRequest ->
                ( { model | uploadEditsRequest = NotAsked }
                , Cmd.none
                , []
                )


updateCache : NominalDate -> MsgCached -> ModelCached -> ( ModelCached, Cmd MsgCached )
updateCache currentDate msg model =
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
                    Json.Decode.decodeString
                        (Json.Decode.map2 (,) (Json.Decode.field "id" decodeEntityId) decodeOfflineSession)
                        offlineSessionJson

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
                CloseSession ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            let
                                newSession =
                                    (\edits -> { session | edits = { edits | explicitlyClosed = True } })
                                        session.edits
                            in
                                ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                                , Cmd.none
                                )
                                    |> sequence (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                MeasurementOutMsgChild childId outMsg ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            let
                                newSession =
                                    makeChildEdit currentDate childId outMsg sessionId session
                            in
                                ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                                , Cmd.none
                                )
                                    |> sequence (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                MeasurementOutMsgMother motherId outMsg ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            let
                                newSession =
                                    makeMotherEdit currentDate motherId outMsg sessionId session
                            in
                                ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                                , Cmd.none
                                )
                                    |> sequence (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                SetCheckedIn motherId checkedIn ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, setCheckedIn checkedIn motherId session ) }
                            , Cmd.none
                            )
                                |> sequence (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                SetChildForm childId form ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, { session | childForms = EveryDict.insert childId form session.childForms } ) }
                            , Cmd.none
                            )
                        )
                        model

                SetMotherForm motherId form ->
                    withEditableSession ( model, Cmd.none )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, { session | motherForms = EveryDict.insert motherId form session.motherForms } ) }
                            , Cmd.none
                            )
                        )
                        model

        SetEditableSession sessionId session ->
            ( { model | editableSession = Success <| Just ( sessionId, session ) }
            , Cmd.none
            )
                |> sequence (updateCache currentDate) [ CacheEditableSession ]


{-| We reach this when the user hits "Save" upon editing something in the measurement
form. So, we want to change the appropriate edit ...
-}
makeChildEdit : NominalDate -> ChildId -> OutMsgChild -> SessionId -> EditableSession -> EditableSession
makeChildEdit currentDate childId outMsg sessionId session =
    -- Clearly, there will be a function that could be abstracted to make
    -- this less verbose, but I shall leave that for the future.
    let
        data =
            getChildMeasurementData childId session
    in
        case outMsg of
            SaveHeight height ->
                let
                    backend =
                        mapMeasurementData .height .height data
                            |> backendValue

                    edit =
                        case backend of
                            -- TODO: Could do a comparison to possibly return to `Unedited`
                            Just value ->
                                Edited
                                    { backend = value
                                    , edited = { value | value = height }
                                    }

                            Nothing ->
                                Created
                                    { participantId = childId
                                    , sessionId = Just sessionId
                                    , dateMeasured = currentDate
                                    , value = height
                                    }
                in
                    mapChildEdits (\edits -> { edits | height = edit }) childId session

            SaveWeight weight ->
                let
                    backend =
                        mapMeasurementData .weight .weight data
                            |> backendValue

                    edit =
                        case backend of
                            Just value ->
                                Edited
                                    { backend = value
                                    , edited = { value | value = weight }
                                    }

                            Nothing ->
                                Created
                                    { participantId = childId
                                    , sessionId = Just sessionId
                                    , dateMeasured = currentDate
                                    , value = weight
                                    }
                in
                    mapChildEdits (\edits -> { edits | weight = edit }) childId session

            SaveMuac muac ->
                let
                    backend =
                        mapMeasurementData .muac .muac data
                            |> backendValue

                    edit =
                        case backend of
                            Just value ->
                                Edited
                                    { backend = value
                                    , edited = { value | value = muac }
                                    }

                            Nothing ->
                                Created
                                    { participantId = childId
                                    , sessionId = Just sessionId
                                    , dateMeasured = currentDate
                                    , value = muac
                                    }
                in
                    mapChildEdits (\edits -> { edits | muac = edit }) childId session

            SaveChildNutritionSigns nutrition ->
                let
                    backend =
                        mapMeasurementData .nutrition .nutrition data
                            |> backendValue

                    edit =
                        case backend of
                            Just value ->
                                Edited
                                    { backend = value
                                    , edited = { value | value = nutrition }
                                    }

                            Nothing ->
                                Created
                                    { participantId = childId
                                    , sessionId = Just sessionId
                                    , dateMeasured = currentDate
                                    , value = nutrition
                                    }
                in
                    mapChildEdits (\edits -> { edits | nutrition = edit }) childId session

            SavePhoto ->
                -- TODO: Re-implement
                session


{-| We reach this when the user hits "Save" upon editing something in the measurement
form. So, we want to change the appropriate edit ...
-}
makeMotherEdit : NominalDate -> MotherId -> OutMsgMother -> SessionId -> EditableSession -> EditableSession
makeMotherEdit currentDate motherId outMsg sessionId session =
    let
        data =
            getMotherMeasurementData motherId session
    in
        case outMsg of
            SaveFamilyPlanningSigns signs ->
                let
                    backend =
                        mapMeasurementData .familyPlanning .familyPlanning data
                            |> backendValue

                    edit =
                        case backend of
                            Just value ->
                                Edited
                                    { backend = value
                                    , edited = { value | value = signs }
                                    }

                            Nothing ->
                                Created
                                    { participantId = motherId
                                    , sessionId = Just sessionId
                                    , dateMeasured = currentDate
                                    , value = signs
                                    }
                in
                    mapMotherEdits (\edits -> { edits | familyPlanning = edit }) motherId session


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
