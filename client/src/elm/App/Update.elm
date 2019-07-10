port module App.Update exposing (init, subscriptions, updateAndThenFetch)

import AnimationFrame
import App.Fetch
import App.Model exposing (..)
import App.Utils exposing (getLoggedInModel)
import Backend.Endpoints exposing (nurseEndpoint)
import Backend.Model
import Backend.Update
import Config
import Date
import Device.Decoder
import Device.Encoder
import Dict
import EveryDict
import Gizra.NominalDate exposing (fromLocalDateTime)
import Http exposing (Error(..))
import HttpBuilder
import Json.Decode exposing (bool, decodeValue, oneOf)
import Json.Encode
import Pages.Device.Model
import Pages.Device.Update
import Pages.People.Update
import Pages.Person.Update
import Pages.PinCode.Model
import Pages.PinCode.Update
import Pages.PrenatalActivity.Model
import Pages.PrenatalActivity.Update
import Pages.PrenatalEncounter.Model
import Pages.PrenatalEncounter.Update
import Pages.Relationship.Model
import Pages.Relationship.Update
import Pages.Session.Model
import Pages.Session.Update
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing ((</>), decodeSingleDrupalEntity, fromEntityId, fromEntityUuid, select, toCmd, toEntityId, toEntityUuid)
import Rollbar
import ServiceWorker.Model
import ServiceWorker.Update
import Task
import Time exposing (minute)
import Translate.Model exposing (Language(..))
import Translate.Utils exposing (languageFromCode, languageToCode)
import Update.Extra exposing (sequence)
import Version
import ZScore.Model
import ZScore.Update


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        activeLanguage =
            case languageFromCode flags.activeLanguage of
                Ok language ->
                    language

                Err msg ->
                    English

        model =
            emptyModel flags

        ( updatedModel, cmd ) =
            case Dict.get flags.hostname Config.configs of
                Just config ->
                    let
                        fetchCachedDevice =
                            HttpBuilder.get "/sw/config/device"
                                |> HttpBuilder.withExpectJson (Device.Decoder.decode config.backendUrl)
                                |> HttpBuilder.toTask
                                |> RemoteData.fromTask
                                |> Task.map
                                    (\response ->
                                        -- We convert 404s to NotAsked ...  if
                                        -- we can't find it locally, we'll ask
                                        -- for a pairing code.
                                        case response of
                                            Failure (BadStatus { status }) ->
                                                if status.code == 404 then
                                                    NotAsked

                                                else
                                                    response

                                            _ ->
                                                response
                                    )
                                |> Task.perform HandlePairedDevice

                        cmd =
                            -- We always check the cache for an offline session, since that affects
                            -- the UI we'll offer to show at a basic level. (An alternative would be
                            -- to fetch it only when we really, really need it).
                            Cmd.batch
                                [ -- We'll leave out the pusherKey for the moment, until we're
                                  -- actually using it.
                                  {- pusherKey
                                     ( config.pusherKey.key
                                     , getClusterName config.pusherKey.cluster
                                     , Pusher.Model.eventNames
                                     )
                                  -}
                                  Task.perform Tick Time.now
                                , fetchCachedDevice
                                ]

                        configuredModel =
                            { config = config
                            , device = Loading
                            , devicePage = Pages.Device.Model.emptyModel
                            , loggedIn = NotAsked
                            , pinCodePage = Pages.PinCode.Model.emptyModel
                            }

                        tryPinCode =
                            if flags.pinCode == "" then
                                []

                            else
                                [ TryPinCode flags.pinCode ]
                    in
                    ( { model | configuration = Success configuredModel }
                    , cmd
                    )
                        |> sequence update
                            (List.append tryPinCode
                                [ MsgServiceWorker (ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Register)
                                , MsgZScore ZScore.Model.FetchAllTables
                                ]
                            )

                Nothing ->
                    ( { model | configuration = Failure <| "No config found for: " ++ flags.hostname }
                    , Cmd.none
                    )
    in
    ( { updatedModel | language = activeLanguage }, cmd )


updateAndThenFetch : Msg -> Model -> ( Model, Cmd Msg )
updateAndThenFetch msg model =
    -- If it's a CheckData message, then `update` will turn this off.
    update msg
        { model | scheduleDataWantedCheck = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentDate =
            fromLocalDateTime <| Date.fromTime model.currentTime

        nurseId =
            getLoggedInModel model
                |> Maybe.map (.nurse >> Tuple.first)
    in
    case msg of
        MsgIndexedDb subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Backend.Update.updateIndexedDb currentDate nurseId subMsg model.indexedDb

                -- Most revisions are handled at the IndexedDB level, but there
                -- is at least one we need to catch here.
                revisionMsgs =
                    case subMsg of
                        Backend.Model.HandleRevisions revisions ->
                            List.filterMap (handleRevision model) revisions

                        _ ->
                            []
            in
            ( { model | indexedDb = subModel }
            , Cmd.map MsgIndexedDb subCmd
            )
                |> sequence update (extraMsgs ++ revisionMsgs)

        MsgLoggedIn loggedInMsg ->
            updateLoggedIn
                (\data ->
                    case loggedInMsg of
                        MsgPageCreatePerson subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.Person.Update.update currentDate subMsg model.indexedDb.people data.createPersonPage
                            in
                            ( { data | createPersonPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageCreatePerson) subCmd
                            , appMsgs
                            )

                        MsgPagePersons subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.People.Update.update subMsg data.personsPage
                            in
                            ( { data | personsPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPagePersons) subCmd
                            , appMsgs
                            )

                        MsgPageRelationship id1 id2 subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.relationshipPages
                                        |> EveryDict.get ( id1, id2 )
                                        |> Maybe.withDefault Pages.Relationship.Model.emptyModel
                                        |> Pages.Relationship.Update.update id1 id2 subMsg
                            in
                            ( { data | relationshipPages = EveryDict.insert ( id1, id2 ) subModel data.relationshipPages }
                            , Cmd.map (MsgLoggedIn << MsgPageRelationship id1 id2) subCmd
                            , extraMsgs
                            )

                        MsgPageSession sessionId subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.sessionPages
                                        |> EveryDict.get sessionId
                                        |> Maybe.withDefault Pages.Session.Model.emptyModel
                                        |> Pages.Session.Update.update sessionId model.indexedDb subMsg
                            in
                            ( { data | sessionPages = EveryDict.insert sessionId subModel data.sessionPages }
                            , Cmd.map (MsgLoggedIn << MsgPageSession sessionId) subCmd
                            , extraMsgs
                            )

                        MsgPagePrenatalEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.prenatalEncounterPages
                                        |> AllDict.get id
                                        |> Maybe.withDefault Pages.PrenatalEncounter.Model.emptyModel
                                        |> Pages.PrenatalEncounter.Update.update subMsg
                            in
                            ( { data | prenatalEncounterPages = AllDict.insert id subModel data.prenatalEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPagePrenatalEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPagePrenatalActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.prenatalActivityPages
                                        |> EveryDict.get ( id, activity )
                                        |> Maybe.withDefault Pages.PrenatalActivity.Model.emptyModel
                                        |> Pages.PrenatalActivity.Update.update subMsg
                            in
                            ( { data | prenatalActivityPages = EveryDict.insert ( id, activity ) subModel data.prenatalActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPagePrenatalActivity id activity) subCmd
                            , extraMsgs
                            )
                )
                model

        MsgPageDevice subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, subCmd, extraMsgs ) =
                            Pages.Device.Update.update subMsg configured.devicePage
                    in
                    ( { configured | devicePage = subModel }
                    , Cmd.map MsgPageDevice subCmd
                    , extraMsgs
                    )
                )
                model

        TryPairingCode code ->
            updateConfigured
                (\configured ->
                    let
                        postCode =
                            HttpBuilder.get (configured.config.backendUrl </> "api/pairing-code" </> code)
                                |> HttpBuilder.withExpectJson (Json.Decode.field "data" (Device.Decoder.decode configured.config.backendUrl))
                                |> HttpBuilder.toTask

                        cacheDevice device =
                            HttpBuilder.put "/sw/config/device"
                                |> HttpBuilder.withJsonBody (Device.Encoder.encode device)
                                |> HttpBuilder.toTask
                                |> Task.map (always device)

                        cmd =
                            -- Try to get the device's access token from the
                            -- backend, and if it succeeds, cache it on the
                            -- service worker
                            postCode
                                |> Task.andThen cacheDevice
                                |> RemoteData.fromTask
                                |> Task.perform HandlePairedDevice
                    in
                    ( { configured | device = Loading }
                    , cmd
                    , []
                    )
                )
                model

        HandlePairedDevice device ->
            updateConfigured
                (\configured ->
                    ( { configured | device = device }
                    , Cmd.none
                    , []
                    )
                )
                model

        MsgPagePinCode subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, subCmd, outMsg ) =
                            Pages.PinCode.Update.update subMsg configured.pinCodePage

                        ( extraMsgs, extraCmds ) =
                            outMsg
                                |> Maybe.map
                                    (\out ->
                                        case out of
                                            Pages.PinCode.Model.TryPinCode code ->
                                                ( [ TryPinCode code ], [] )

                                            Pages.PinCode.Model.Logout ->
                                                ( [ SetLoggedIn NotAsked, SetHealthCenter Nothing ]
                                                , [ cachePinCode "", cacheHealthCenter "" ]
                                                )

                                            Pages.PinCode.Model.GoToRandomPrenatalEncounter ->
                                                ( [ MsgIndexedDb Backend.Model.GoToRandomPrenatalEncounter ], [] )

                                            Pages.PinCode.Model.SetActivePage page ->
                                                ( [ SetActivePage page ], [] )

                                            Pages.PinCode.Model.SetHealthCenter healthCenterId ->
                                                ( [ SetHealthCenter (Just healthCenterId) ], [] )
                                    )
                                |> Maybe.withDefault ( [], [] )
                    in
                    ( { configured | pinCodePage = subModel }
                    , Cmd.batch (Cmd.map MsgPagePinCode subCmd :: extraCmds)
                    , extraMsgs
                    )
                )
                model

        MsgServiceWorker subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    ServiceWorker.Update.update model.currentTime subMsg model.serviceWorker
            in
            ( { model | serviceWorker = subModel }
            , Cmd.map MsgServiceWorker subCmd
            )
                |> sequence update extraMsgs

        MsgZScore subMsg ->
            let
                ( subModel, subCmd ) =
                    ZScore.Update.update subMsg model.zscores
            in
            ( { model | zscores = subModel }
            , Cmd.map MsgZScore subCmd
            )

        SetActivePage page ->
            ( { model | activePage = page }
            , Cmd.none
            )

        SendRollbar level message data ->
            updateConfigured
                (\configured ->
                    let
                        version =
                            Version.version
                                |> .build
                                |> Json.Encode.string

                        cmd =
                            Rollbar.send
                                configured.config.rollbarToken
                                (Rollbar.scope "user")
                                (Rollbar.environment configured.config.name)
                                0
                                level
                                message
                                (Dict.insert "build" version data)
                                |> Task.attempt HandleRollbar
                    in
                    ( configured
                    , cmd
                    , []
                    )
                )
                model

        HandleRollbar result ->
            -- For now, we do nothing
            ( model, Cmd.none )

        SetLanguage language ->
            ( { model | language = language }
            , setLanguage <| languageToCode language
            )

        SetPersistentStorage data ->
            ( { model | persistentStorage = Just data }
            , Cmd.none
            )

        SetMemoryQuota quota ->
            ( { model | memoryQuota = Just quota }
            , Cmd.none
            )

        SetStorageQuota quota ->
            ( { model | storageQuota = Just quota }
            , Cmd.none
            )

        SetHealthCenter healthCenterId ->
            ( { model | healthCenterId = healthCenterId }
            , healthCenterId
                |> Maybe.map fromEntityUuid
                |> Maybe.withDefault ""
                |> cacheHealthCenter
            )

        Tick time ->
            let
                extraMsgs =
                    case model.serviceWorker.lastUpdateCheck of
                        Nothing ->
                            [ MsgServiceWorker <| ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Update ]

                        Just checked ->
                            -- Automatically check for updates every hour
                            if time - checked > 60 * Time.minute then
                                [ MsgServiceWorker <| ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Update ]

                            else
                                []
            in
            ( { model | currentTime = time }
            , Cmd.none
            )
                |> sequence update extraMsgs

        TrySyncing ->
            -- Normally handled automatically, but sometimes nice to trigger manually
            ( model, trySyncing () )

        TryPinCode code ->
            updateConfigured
                (\configured ->
                    let
                        formatResponse =
                            .items >> List.head >> Maybe.map RemoteData.succeed >> Maybe.withDefault (RemoteData.Failure NetworkError)

                        checkPinCode =
                            select "/sw" nurseEndpoint { pinCode = Just code }
                                |> toCmd (RemoteData.fromResult >> RemoteData.andThen formatResponse >> SetLoggedIn)
                    in
                    ( { configured | loggedIn = Loading }
                    , Cmd.batch
                        [ checkPinCode
                        , cachePinCode code
                        ]
                    , []
                    )
                )
                model

        -- Note that this also resets any data which depends on being logged in.
        SetLoggedIn nurse ->
            updateConfigured
                (\configured ->
                    ( { configured | loggedIn = RemoteData.map emptyLoggedInModel nurse }
                    , Cmd.none
                    , []
                    )
                )
                model

        CheckDataWanted ->
            -- Note that we will be called repeatedly. So, it's vitally important that
            -- the `fetch` implementations use a `WebData`-like strategy to indicate
            -- that a request is in progress, and doesn't need to be triggered again.
            -- Otherwise, we'll keep issuing the same requests, over and over.
            let
                -- These are messages to fetch the data we want now, without
                -- considering whether we already have it.
                dataNowWanted =
                    App.Fetch.fetch model

                -- These are the messages we should actually issue to fetch data now.
                -- As an improvement, we could compare with `model.dataWanted` to see
                -- whether the msg is **newly** desired ... that is, whether its status
                -- just flipped. We could treat newly desired data differently. For
                -- instance, we might try to re-fetch it even in a `Failure` state.
                -- (Since that wouldn't infinitely repeat).
                dataToFetch =
                    List.filter (App.Fetch.shouldFetch model) dataNowWanted

                -- Update our existing dataWanted to indicate that the data now wanted
                -- was last wanted now.
                dataWanted =
                    List.foldl (\msg -> EveryDict.insert msg model.currentTime) model.dataWanted dataNowWanted

                fiveMinutes =
                    5 * 1000 * 60

                -- Figure out what to remember and what to forget.
                ( dataToForget, dataToRemember ) =
                    EveryDict.partition (\_ lastWanted -> model.currentTime - lastWanted > fiveMinutes) dataWanted

                -- Our new base model, remembering the desired data, and forgetting
                -- the data to forget.
                newModel =
                    dataToForget
                        |> EveryDict.keys
                        |> List.foldl App.Fetch.forget
                            { model
                                | dataWanted = dataToRemember
                                , scheduleDataWantedCheck = False
                            }
            in
            sequence update dataToFetch ( newModel, Cmd.none )


{-| Updates our `nurse` user if the uuid matches the logged-in user.
-}
handleRevision : Model -> Backend.Model.Revision -> Maybe Msg
handleRevision model revision =
    case revision of
        Backend.Model.NurseRevision uuid data ->
            Maybe.andThen
                (\loggedIn ->
                    if Tuple.first loggedIn.nurse == uuid then
                        Just (SetLoggedIn (Success ( uuid, data )))

                    else
                        Nothing
                )
                (getLoggedInModel model)

        _ ->
            Nothing


{-| Convenience function to process a msg which depends on having a configuration.

The function you supply returns a third parameter, which is a list of additional messages to process.

-}
updateConfigured : (ConfiguredModel -> ( ConfiguredModel, Cmd Msg, List Msg )) -> Model -> ( Model, Cmd Msg )
updateConfigured func model =
    model.configuration
        |> RemoteData.map
            (\configured ->
                let
                    ( subModel, cmd, extraMsgs ) =
                        func configured
                in
                sequence update
                    extraMsgs
                    ( { model | configuration = Success subModel }
                    , cmd
                    )
            )
        |> RemoteData.withDefault ( model, Cmd.none )


{-| Convenience function to process a msg which depends on being logged in.
-}
updateLoggedIn : (LoggedInModel -> ( LoggedInModel, Cmd Msg, List Msg )) -> Model -> ( Model, Cmd Msg )
updateLoggedIn func model =
    updateConfigured
        (\configured ->
            configured.loggedIn
                |> RemoteData.map
                    (\loggedIn ->
                        let
                            ( subModel, cmd, extraMsgs ) =
                                func loggedIn
                        in
                        ( { configured | loggedIn = Success subModel }
                        , cmd
                        , extraMsgs
                        )
                    )
                |> RemoteData.withDefault
                    ( configured, Cmd.none, [] )
        )
        model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        checkDataWanted =
            if model.scheduleDataWantedCheck then
                [ AnimationFrame.times (always CheckDataWanted) ]

            else
                []
    in
    Sub.batch
        ([ Time.every minute Tick
         , Sub.map MsgServiceWorker ServiceWorker.Update.subscriptions
         , persistentStorage SetPersistentStorage
         , storageQuota SetStorageQuota
         , memoryQuota SetMemoryQuota
         ]
            ++ checkDataWanted
        )


{-| Saves PIN code entered by user, so that we can use it again if
the browser is reloaded.
-}
port cachePinCode : String -> Cmd msg


{-| Manually kick off a sync event. Normally, handled automatically.
-}
port trySyncing : () -> Cmd msg


{-| Send Pusher key and cluster to JS.
-}
port pusherKey : ( String, String, List String ) -> Cmd msg


{-| Set the user's current language.
-}
port setLanguage : String -> Cmd msg


{-| Let the Javascript tell us if we've successfully requested persistent
storage.
-}
port persistentStorage : (Bool -> msg) -> Sub msg


{-| Let the Javascript tell us about memory quotas.
-}
port memoryQuota : (MemoryQuota -> msg) -> Sub msg


{-| Let the Javascript tell us about our storage quota.
-}
port storageQuota : (StorageQuota -> msg) -> Sub msg


{-| Saves Health center ID selected by user, so that we can use it again if
the browser is reloaded.
-}
port cacheHealthCenter : String -> Cmd msg
