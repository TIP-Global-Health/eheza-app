port module App.Update exposing (init, subscriptions, updateAndThenFetch)

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
import Pages.Admin.Update
import Pages.Device.Model
import Pages.Device.Update
import Pages.Page exposing (Page(..), UserPage(AdminPage, ClinicsPage))
import Pages.PinCode.Model
import Pages.PinCode.Update
import Pages.Session.Model
import Pages.Session.Update
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing ((</>), decodeSingleDrupalEntity, select, toCmd, toEntityId)
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
    update msg model
        |> App.Fetch.andThenFetch update


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
                ( subModel, subCmd ) =
                    Backend.Update.updateIndexedDb currentDate nurseId subMsg model.indexedDb

                -- Most revisions are handled at the IndexedDB level, but there
                -- is at least one we need to catch here.
                extraMsgs =
                    case subMsg of
                        Backend.Model.HandleRevisions revisions ->
                            List.filterMap (handleRevision model) revisions

                        _ ->
                            []
            in
            ( { model | indexedDb = subModel }
            , Cmd.map MsgIndexedDb subCmd
            )
                |> sequence update extraMsgs

        MsgLoggedIn loggedInMsg ->
            updateLoggedIn
                (\data ->
                    case loggedInMsg of
                        MsgBackend subMsg ->
                            let
                                ( backend, cmd ) =
                                    -- TODO: delete this
                                    Backend.Update.updateBackend "" "" subMsg data.backend
                            in
                            ( { data | backend = backend }
                            , Cmd.map (MsgLoggedIn << MsgBackend) cmd
                            , []
                            )

                        MsgPageAdmin subMsg ->
                            let
                                ( newModel, cmd, appMsgs ) =
                                    Pages.Admin.Update.update currentDate data.backend subMsg data.adminPage
                            in
                            ( { data | adminPage = newModel }
                            , Cmd.map (MsgLoggedIn << MsgPageAdmin) cmd
                            , appMsgs
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
                                                ( [ SetLoggedIn NotAsked ], [ cachePinCode "" ] )

                                            Pages.PinCode.Model.SetActivePage page ->
                                                ( [ SetActivePage page ], [] )
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
            let
                -- We reset certain requests if we're navigating to the admin
                -- page from elsewhere.
                resetSessionRequests =
                    if page == UserPage AdminPage && page /= model.activePage then
                        [ MsgLoggedIn <| MsgBackend <| Backend.Model.ResetSessionRequests ]

                    else
                        []
            in
            sequence update
                resetSessionRequests
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
    Sub.batch
        [ Time.every minute Tick
        , Sub.map MsgServiceWorker ServiceWorker.Update.subscriptions
        , persistentStorage SetPersistentStorage
        , storageQuota SetStorageQuota
        , memoryQuota SetMemoryQuota
        ]


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
