port module Backend.Update exposing (fetchEditableSession, subscriptions, updateBackend, updateCache, updateIndexedDb)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Activity.Utils exposing (setCheckedIn)
import App.Model
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeMeasurementEdits)
import Backend.Measurement.Encoder exposing (encodeMeasurementEdits)
import Backend.Measurement.Model exposing (Edit(..))
import Backend.Measurement.Utils exposing (backendValue, getPhotosToUpload, mapMeasurementData)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeOfflineSession, decodeSession, decodeTrainingSessionRequest)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId, encodeSession, encodeTrainingSessionRequest)
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..), OfflineSession, Session)
import Backend.Session.Utils exposing (getChildMeasurementData, getMotherMeasurementData, getPhotoUrls, makeEditableSession, mapChildEdits, mapMotherEdits, setPhotoFileId)
import Backend.Utils exposing (withEditableSession)
import CacheStorage.Model exposing (cachePhotos, clearCachedPhotos)
import CacheStorage.Update
import Config.Model exposing (BackendUrl)
import Dict exposing (Dict)
import EveryDict
import EveryDictList
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Http exposing (Error)
import HttpBuilder
import Json.Decode exposing (Decoder, field, succeed)
import Json.Encode exposing (Value, object)
import Json.Encode.Extra
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)
import Rollbar
import Utils.WebData exposing (resetError, resetSuccess)


updateIndexedDb : MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb )
updateIndexedDb msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        FetchClinics ->
            ( { model | clinics = Loading }
            , sw.select clinicEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedClinics)
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
            , Cmd.none
            )

        FetchHealthCenters ->
            ( { model | healthCenters = Loading }
            , sw.select healthCenterEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedHealthCenters)
            )

        HandleFetchedHealthCenters data ->
            ( { model | healthCenters = data }
            , Cmd.none
            )

        FetchSyncData ->
            ( { model | syncData = Loading }
            , sw.select syncDataEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSyncData)
            )

        HandleFetchedSyncData data ->
            ( { model | syncData = data }
            , Cmd.none
            )

        FetchSessionsByClinic clinicId ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId Loading model.sessionsByClinic }
            , sw.select sessionEndpoint { clinic = Just clinicId }
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSessionsByClinic clinicId)
            )

        HandleFetchedSessionsByClinic clinicId data ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId data model.sessionsByClinic }
            , Cmd.none
            )

        HandleRevisions revisions ->
            ( List.foldl handleRevision model revisions
            , Cmd.none
            )

        SaveSyncData uuid data ->
            ( model
            , sw.put syncDataEndpoint uuid data
                |> withoutDecoder
                |> toCmd (always IgnoreResponse)
            )

        DeleteSyncData uuid ->
            ( model
            , sw.delete syncDataEndpoint uuid
                |> toCmd (always IgnoreResponse)
            )

        IgnoreResponse ->
            ( model, Cmd.none )


handleRevision : Revision -> ModelIndexedDb -> ModelIndexedDb
handleRevision revision model =
    case revision of
        HealthCenterRevision uuid data ->
            let
                -- We don't do anything with revisions until we've fetched
                -- some original data.
                healthCenters =
                    RemoteData.map (EveryDictList.insert uuid data) model.healthCenters
            in
            { model | healthCenters = healthCenters }

        ClinicRevision uuid data ->
            let
                clinics =
                    RemoteData.map (EveryDictList.insert uuid data) model.clinics
            in
            { model | clinics = clinics }

        SessionRevision uuid data ->
            -- First, remove the session from all clinics (it might previously have been
            -- in any). Then, add it in the right place.
            let
                sessionsByClinic =
                    model.sessionsByClinic
                        |> EveryDict.map (always (RemoteData.map (EveryDictList.remove uuid)))
                        |> EveryDict.update data.clinicId (Maybe.map (RemoteData.map (EveryDictList.insert uuid data)))
            in
            { model | sessionsByClinic = sessionsByClinic }

        _ ->
            model


updateBackend : BackendUrl -> String -> MsgBackend -> ModelBackend -> ( ModelBackend, Cmd MsgBackend, List MsgCached )
updateBackend backendUrl accessToken msg model =
    let
        crud =
            applyBackendUrl backendUrl
                |> applyAccessToken accessToken

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
        PostSession session ->
            ( { model | postSessionRequest = Loading }
            , crud.post sessionEndpoint session
                |> toCmd (RemoteData.fromResult >> HandlePostedSession)
            , []
            )

        PostTrainingSessionRequest request ->
            ( { model | postTrainingSessionRequest = Loading }
            , crud.post trainingSessionsEndpoint request
                -- We use the Tuple.second becausw we're only interested the
                -- value ... the backend doesn't (currently) send a key.
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.second >> HandleTrainingSessionResponse)
            , []
            )

        HandleTrainingSessionResponse webdata ->
            let
                futureSessions =
                    case webdata of
                        Success _ ->
                            -- This will trigger the lazy load of the created sessions
                            -- (or the sessions remaining after deletion).
                            NotAsked

                        _ ->
                            model.futureSessions

                newModel =
                    { model
                        | postTrainingSessionRequest = webdata
                        , futureSessions = futureSessions
                    }
            in
            ( newModel, Cmd.none, [] )

        HandlePostedSession webdata ->
            let
                newModel =
                    case webdata of
                        Success ( sessionId, session ) ->
                            -- We'll unconditionally insert this into
                            -- futureSessions at the moment, to show
                            -- success ... if we cache data differently at
                            -- some point we'll need to change this.
                            let
                                futureSessions =
                                    RemoteData.map
                                        (Tuple.mapSecond (EveryDictList.insert sessionId session))
                                        model.futureSessions
                            in
                            { model
                                | postSessionRequest = webdata
                                , futureSessions = futureSessions
                            }

                        _ ->
                            { model | postSessionRequest = webdata }
            in
            ( newModel, Cmd.none, [] )

        FetchFutureSessions date ->
            ( model, Cmd.none, [] )

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
            , crud.get offlineSessionEndpoint sessionId
                |> toCmd (Result.map (\session -> ( sessionId, session )) >> HandleFetchedOfflineSessionFromBackend)
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
                        -- We immediately kick off a save into the cache, and to cache the photos we'll need
                        ( { model | offlineSessionRequest = Success sessionId }
                        , Cmd.none
                        , [ SetEditableSession sessionId (makeEditableSession session)
                          , MsgCacheStorage <| cachePhotos <| getPhotoUrls session
                          ]
                        )

        -- Like FetchOfflineSessionFromBackend, but just tries to fetch in
        -- the background ...  doesn't complain if it doesn't work.  We'll
        -- do this on reload, to pick up any changes made in the admin UI
        -- on the backend. It can be done quite simply, because we don't
        -- mutate the offlineSession ... we can just substitute it in.  An
        -- alternative would be to push changes to clients, but that's a
        -- bit tricky when we're contemplating periods offline ... see
        -- disucssion at <https://github.com/Gizra/ihangane/issues/436>
        RefetchOfflineSession sessionId ->
            ( model
            , crud.get offlineSessionEndpoint sessionId
                |> toCmd (Result.map (\session -> ( sessionId, session )) >> HandleRefetchedOfflineSession)
            , []
            )

        HandleRefetchedOfflineSession result ->
            resetErrorsIfOk result <|
                case result of
                    Err error ->
                        -- We just ignore errors ... we may well be
                        -- offline, which is fine.
                        ( model, Cmd.none, [] )

                    Ok ( sessionId, session ) ->
                        -- We immediately kick off a save into the cache,
                        -- and to cache the photos we'll need.  The photo
                        -- URLs appear to change when the photo changes, so
                        -- we have code in app.js that won't re-cache a
                        -- photo we already have.
                        ( model
                        , Cmd.none
                        , [ SetOfflineSession sessionId session
                          , MsgCacheStorage <| cachePhotos <| getPhotoUrls session
                          ]
                        )

        ResetErrors ->
            -- Reset some error conditions to `NotAsked`, so that they will
            -- be automatically retried if needed.
            ( { model | futureSessions = resetError model.futureSessions }
            , Cmd.none
            , []
            )

        ResetSessionRequests ->
            -- Reset session requests to `NotAsked` if `Error` or `Success`.
            -- This is for requests where we're showing an  indication in the
            -- UI, and we want to stop doing that at certain moments.
            ( { model
                | postSessionRequest = resetError <| resetSuccess model.postSessionRequest
                , postTrainingSessionRequest = resetError <| resetSuccess model.postTrainingSessionRequest
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
            -- For now at least, our strategy is this:
            --
            -- 1. Get the photos we need to upload.
            -- 2. If there are some, upload the first one.
            -- 3. If not, upload the actual edits.
            --
            -- The response from trying to upload a photo will call back to
            -- here, so we'll either upload the next photo, or upload the
            -- edits themselves if we're done. Basically, a kind of
            -- asynchronous recursion, I suppose.
            --
            -- There may be a more sensible way of doing this ... for instance
            -- we could try uploading photos in parrallel? But this is
            -- fairly comprehensible.
            case getPhotosToUpload edits of
                first :: _ ->
                    -- We still have one to upload, so kick off a request.
                    --
                    -- TODO: We could be more sophisticated with `uploadEditsRequest`
                    -- to show exactly what stage we're at ... e.g. how many photos
                    -- are remaining?
                    ( { model | uploadEditsRequest = Loading }
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (updateBackend backendUrl accessToken)
                            (List.map UploadPhoto [ first ])

                [] ->
                    -- All photos have been uploaded, so actually upload the edits
                    ( { model | uploadEditsRequest = Loading }
                    , crud.patchAny offlineSessionEndpoint sessionId (encodeMeasurementEdits edits)
                        |> withoutDecoder
                        |> toCmd (HandleUploadedEdits sessionId)
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

        UploadPhoto photo ->
            -- This is a bit of a special HTTP request, so we don't use
            -- the ordinary endpoints.
            let
                json =
                    object
                        [ ( "backendUrl", Json.Encode.string backendUrl )
                        , ( "accessToken", Json.Encode.string accessToken )
                        , ( "cachedUrl", Json.Encode.string photo.value.url )
                        ]

                decoder =
                    -- We expect what Drupal returns when you upload a file.
                    decodeSingleDrupalEntity (field "id" decodeInt)

                cmd =
                    HttpBuilder.post "backend-upload/images"
                        |> HttpBuilder.withJsonBody json
                        |> HttpBuilder.withExpect (Http.expectJson decoder)
                        |> HttpBuilder.send (HandleUploadPhotoResponse photo)
            in
            ( model
            , cmd
            , []
            )

        HandleUploadPhotoResponse photo result ->
            case result of
                Err err ->
                    -- If we get an error, record that in our `uploadEditsRequest`
                    ( { model | uploadEditsRequest = Failure err }
                    , Cmd.none
                    , []
                    )

                Ok fileId ->
                    -- So, first we need to update our editable session to record that
                    -- this photo now has a fileId. That needs to be cached, so that
                    -- we don't upload the photo again (assuming the page gets reloaded
                    -- etc.). Then, we want to try uploading the edits again, which will
                    -- either upload the next photo, or actually upload the edits, if
                    -- we're done.
                    --
                    -- Then, we kick off another request to upload the edits. We need to
                    -- do that via MsgCached, because we don't actually know
                    -- what the session is here ...
                    ( model
                    , Cmd.none
                    , [ MsgEditableSession <| SetPhotoFileId photo fileId
                      , ContinueUploadingEdits
                      ]
                    )


updateCache : NominalDate -> MsgCached -> ModelCached -> ( ModelCached, Cmd MsgCached, List App.Model.Msg )
updateCache currentDate msg model =
    case msg of
        CacheEditableSession ->
            withEditableSession ( model, Cmd.none, [] )
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
                    , []
                    )
                )
                model

        CacheEditableSessionResult result ->
            -- TODO: Actually do something with the result. For now, we just mark Success.
            withEditableSession ( model, Cmd.none, [] )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Success () } ) }
                    , Cmd.none
                    , []
                    )
                )
                model

        CacheEdits ->
            withEditableSession ( model, Cmd.none, [] )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Loading } ) }
                    , encodeMeasurementEdits session.edits
                        |> Json.Encode.encode 0
                        |> cacheEdits
                    , []
                    )
                )
                model

        CacheEditsResult result ->
            -- TODO: Actually consult the result ...
            withEditableSession ( model, Cmd.none, [] )
                (\sessionId session ->
                    ( { model | editableSession = Success <| Just ( sessionId, { session | update = Success () } ) }
                    , Cmd.none
                    , []
                    )
                )
                model

        ContinueUploadingEdits ->
            withEditableSession ( model, Cmd.none, [] )
                (\sessionId session ->
                    ( model
                    , Cmd.none
                    , [ UploadEdits sessionId session.edits
                            |> App.Model.MsgBackend
                            |> App.Model.MsgLoggedIn
                      ]
                    )
                )
                model

        DeleteEditableSession ->
            ( { model | editableSession = Success Nothing }
            , deleteEditableSession ()
            , []
            )
                |> sequenceExtra (updateCache currentDate)
                    [ MsgCacheStorage clearCachedPhotos ]

        FetchEditableSessionFromCache ->
            ( { model | editableSession = Loading }
            , fetchEditableSession ()
            , []
            )

        -- We just get this at startup time. So, we also kick off a re-check
        -- to see if the offline session has changed.
        HandleEditableSession ( offlineSessionJson, editsJson ) ->
            let
                decodedOfflineSession =
                    if offlineSessionJson == "" then
                        -- If the port gives us an empty string, then there was
                        -- nothing found in local storage. This is fine ...  it
                        -- just means we don't have any. So, we indicate an Ok
                        -- result, but nothing found.
                        Ok Nothing

                    else
                        -- If local storage had something other than an empty
                        -- string, we try to decode it. This should succeed, so
                        -- we indicate an error if it doesn't. If it does
                        -- succeed, we wrap it in `Just`.
                        Json.Decode.decodeString
                            (Json.Decode.map2 (,) (Json.Decode.field "id" decodeEntityUuid) decodeOfflineSession)
                            offlineSessionJson
                            |> Result.map Just

                decodedEdits =
                    if editsJson == "" then
                        -- If the port gave us an empty string for editsJson,
                        -- that means nothing was found in local storage. This
                        -- is fine ... it just means we don't have any. So, we
                        -- indicate an OK result, but Nothing found.
                        Ok Nothing

                    else
                        -- If we got something other than an empty string, we
                        -- try to decode it. We wrap it in a `Just` -- that
                        -- way, we either succeed in decoding and get actual
                        -- edits, or we fail in decoding (where we should have
                        -- succeeded, so it's an error).
                        Json.Decode.decodeString decodeMeasurementEdits editsJson
                            |> Result.map Just

                decodedEditableSession =
                    case ( decodedOfflineSession, decodedEdits ) of
                        ( Ok Nothing, Ok Nothing ) ->
                            -- If both were absent from local storage, then
                            -- that's normal ... we just don't have a cached
                            -- session.
                            Success Nothing

                        ( Ok (Just ( sessionId, offlineSession )), Ok Nothing ) ->
                            -- If we have the offline session, but there were
                            -- no edits in local storage, then we can start
                            -- with some blank edits.
                            ( sessionId, makeEditableSession offlineSession )
                                |> Just
                                |> Success

                        ( Ok Nothing, Ok (Just _) ) ->
                            -- If we have the edits, but no offline session, then
                            -- something has gone wrong. So, we indicate that.
                            FoundEditsButNoSession
                                { editsJson = editsJson }
                                |> Failure

                        ( Ok (Just ( sessionId, offlineSession )), Ok (Just edits) ) ->
                            -- We've got both, so this is the happy path
                            makeEditableSession offlineSession
                                |> (\session ->
                                        ( sessionId
                                        , { session | edits = edits }
                                        )
                                   )
                                |> Just
                                |> Success

                        ( Err offlineSessionError, Ok _ ) ->
                            DecodersFailed
                                { editsJson = editsJson
                                , editsError = Nothing
                                , offlineSessionJson = offlineSessionJson
                                , offlineSessionError = Just offlineSessionError
                                }
                                |> Failure

                        ( Ok _, Err editsError ) ->
                            DecodersFailed
                                { editsJson = editsJson
                                , editsError = Just editsError
                                , offlineSessionJson = offlineSessionJson
                                , offlineSessionError = Nothing
                                }
                                |> Failure

                        ( Err offlineSessionError, Err editsError ) ->
                            DecodersFailed
                                { editsJson = editsJson
                                , editsError = Just editsError
                                , offlineSessionJson = offlineSessionJson
                                , offlineSessionError = Just offlineSessionError
                                }
                                |> Failure

                msgs =
                    case decodedEditableSession of
                        Success (Just ( sessionId, _ )) ->
                            -- This is where we're re-checking to see if the backend
                            -- has any updates to the offlineSession.
                            [ RefetchOfflineSession sessionId
                                |> App.Model.MsgBackend
                                |> App.Model.MsgLoggedIn
                            ]

                        Success Nothing ->
                            []

                        Failure err ->
                            [ App.Model.SendRollbar Rollbar.Error "Error getting session from local storage" (encodeForRollbar err)
                            ]

                        NotAsked ->
                            []

                        Loading ->
                            []
            in
            ( { model | editableSession = decodedEditableSession }
            , Cmd.none
            , msgs
            )

        MsgCacheStorage subMsg ->
            let
                ( subModel, subCmd ) =
                    CacheStorage.Update.update subMsg model.cacheStorage
            in
            ( { model | cacheStorage = subModel }
            , Cmd.map MsgCacheStorage subCmd
            , []
            )

        MsgEditableSession subMsg ->
            case subMsg of
                CloseSession ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            let
                                newSession =
                                    (\edits -> { session | edits = { edits | explicitlyClosed = True } })
                                        session.edits
                            in
                            ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                            , Cmd.none
                            , []
                            )
                                |> sequenceExtra (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                MeasurementOutMsgChild childId outMsg ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            let
                                newSession =
                                    makeChildEdit currentDate childId outMsg sessionId session
                            in
                            ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                            , Cmd.none
                            , []
                            )
                                |> sequenceExtra (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                MeasurementOutMsgMother motherId outMsg ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            let
                                newSession =
                                    makeMotherEdit currentDate motherId outMsg sessionId session
                            in
                            ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                            , Cmd.none
                            , []
                            )
                                |> sequenceExtra (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                RefetchSession ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId _ ->
                            ( model
                            , Cmd.none
                            , [ RefetchOfflineSession sessionId
                                    |> App.Model.MsgBackend
                                    |> App.Model.MsgLoggedIn
                              ]
                            )
                        )
                        model

                SetCheckedIn motherId checkedIn ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, setCheckedIn checkedIn motherId session ) }
                            , Cmd.none
                            , []
                            )
                                |> sequenceExtra (updateCache currentDate) [ CacheEdits ]
                        )
                        model

                SetChildForm childId form ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, { session | childForms = EveryDict.insert childId form session.childForms } ) }
                            , Cmd.none
                            , []
                            )
                        )
                        model

                SetMotherForm motherId form ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, { session | motherForms = EveryDict.insert motherId form session.motherForms } ) }
                            , Cmd.none
                            , []
                            )
                        )
                        model

                SetPhotoFileId photo id ->
                    withEditableSession ( model, Cmd.none, [] )
                        (\sessionId session ->
                            ( { model | editableSession = Success <| Just ( sessionId, setPhotoFileId photo id session ) }
                            , Cmd.none
                            , []
                            )
                                |> sequenceExtra (updateCache currentDate) [ CacheEdits ]
                        )
                        model

        SetEditableSession sessionId session ->
            ( { model | editableSession = Success <| Just ( sessionId, session ) }
            , Cmd.none
            , []
            )
                |> sequenceExtra (updateCache currentDate) [ CacheEditableSession ]

        -- Like SetEditableSession, but we just substitute the offlineSesttion part.
        -- This works because we never mutate the offlineSession locally.
        SetOfflineSession sessionId offlineSession ->
            withEditableSession ( model, Cmd.none, [] )
                (\currentId currentSession ->
                    if sessionId == currentId then
                        let
                            newSession =
                                { currentSession | offlineSession = offlineSession }
                        in
                        ( { model | editableSession = Success <| Just ( sessionId, newSession ) }
                        , Cmd.none
                        , []
                        )
                            |> sequenceExtra (updateCache currentDate) [ CacheEditableSession ]

                    else
                        ( model, Cmd.none, [] )
                )
                model


encodeForRollbar : CachedSessionError -> Dict String Value
encodeForRollbar err =
    case err of
        FoundEditsButNoSession { editsJson } ->
            Dict.fromList
                [ ( "type", Json.Encode.string "Found edits but no session" )
                , ( "edits", Json.Encode.string editsJson )
                ]

        DecodersFailed details ->
            -- We send the full edits because it's nice to save that, and it
            -- doesn't contain names. We don't send the full offline session,
            -- because it's immutable, so we don't need to save it, and the
            -- JSOn error itself typically contains enough information to see
            -- what went wrong.
            Dict.fromList
                [ ( "type", Json.Encode.string "Decoders failed" )
                , ( "edits", Json.Encode.string details.editsJson )
                , ( "editsError", Json.Encode.Extra.maybe Json.Encode.string details.editsError )
                , ( "offlineSessionError", Json.Encode.Extra.maybe Json.Encode.string details.offlineSessionError )
                ]


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

        SavePhoto photo ->
            let
                backend =
                    mapMeasurementData .photo .photo data
                        |> backendValue

                edit =
                    case backend of
                        Just value ->
                            Edited
                                { backend = value
                                , edited = { value | value = photo }
                                }

                        Nothing ->
                            Created
                                { participantId = childId
                                , sessionId = Just sessionId
                                , dateMeasured = currentDate
                                , value = photo
                                }
            in
            mapChildEdits (\edits -> { edits | photo = edit }) childId session


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
        , Sub.map MsgCacheStorage CacheStorage.Update.subscriptions
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
