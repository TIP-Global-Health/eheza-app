port module App.Update exposing (init, loginConfig, subscriptions, update)

import App.Model exposing (..)
import Backend.Model
import Backend.Session.Model
import Backend.Update
import Backend.Utils exposing (withEditableSession)
import Config
import Date
import Device.Decoder
import Device.Encoder
import Dict
import EverySet
import Gizra.NominalDate exposing (fromLocalDateTime)
import Http exposing (Error(..))
import HttpBuilder
import Json.Decode exposing (bool, decodeValue, oneOf)
import Json.Encode
import Maybe.Extra
import Pages.Admin.Update
import Pages.Device.Model
import Pages.Device.Update
import Pages.Login.Model
import Pages.Login.Update
import Pages.Model
import Pages.Page exposing (Page(..), UserPage(AdminPage, ClinicsPage))
import Pages.Update
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing ((</>), decodeSingleDrupalEntity)
import Restful.Login exposing (AuthenticatedUser, Credentials, LoginEvent(..), UserAndData(..), checkCachedCredentials)
import Rollbar
import ServiceWorker.Model
import ServiceWorker.Update
import Task
import Time exposing (minute)
import Translate exposing (Language(..), languageFromCode, languageToCode)
import Update.Extra exposing (sequence)
import User.Decoder exposing (decodeUser)
import User.Encoder exposing (encodeUser)
import User.Model exposing (Role(Administrator), User)
import Version
import ZScore.Model
import ZScore.Update


loginConfig : Restful.Login.Config () User LoggedInModel Msg
loginConfig =
    -- The `oneOf` below is necessary because how the backend sends the user is
    -- a little different from how we encode it for local storage ... this
    -- could possibly be solved another way.
    Restful.Login.drupalConfig
        { decodeUser = oneOf [ decodeSingleDrupalEntity decodeUser, decodeUser ]
        , encodeUser = Just encodeUser
        , initialAnonymousData = ()
        , initialAuthenticatedData = \_ _ -> emptyLoggedInModel
        , cacheCredentials = curry cacheCredentials
        , tag = MsgLogin
        }


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
                        ( loginStatus, loginCmd ) =
                            -- We kick off the process of checking the cached credentials which
                            -- we were provided.
                            checkCachedCredentials loginConfig config.backendUrl (Just flags.credentials)

                        fetchCachedDevice =
                            HttpBuilder.get "/sw/config/device"
                                |> HttpBuilder.withExpectJson Device.Decoder.decode
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
                                , loginCmd
                                , fetchCachedDevice
                                , Backend.Update.fetchEditableSession ()
                                ]

                        configuredModel =
                            { config = config
                            , loginPage = Pages.Login.Model.emptyModel
                            , login = loginStatus
                            , device = Loading
                            , devicePage = Pages.Device.Model.emptyModel
                            }
                    in
                    ( { model | configuration = Success configuredModel }
                    , cmd
                    )
                        |> sequence update
                            [ MsgServiceWorker (ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Register)
                            , MsgZScore ZScore.Model.FetchAllTables
                            ]

                Nothing ->
                    ( { model | configuration = Failure <| "No config found for: " ++ flags.hostname }
                    , Cmd.none
                    )
    in
    ( { updatedModel | language = activeLanguage }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentDate =
            fromLocalDateTime <| Date.fromTime model.currentTime
    in
    case msg of
        MsgCache subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Backend.Update.updateCache currentDate subMsg model.cache
            in
            ( { model | cache = subModel }
            , Cmd.map MsgCache subCmd
            )
                |> sequence update extraMsgs

        MsgLoggedIn loggedInMsg ->
            updateLoggedIn
                (\credentials data ->
                    case loggedInMsg of
                        MsgBackend subMsg ->
                            let
                                ( backend, cmd, cacheMsgs ) =
                                    Backend.Update.updateBackend credentials.backendUrl credentials.accessToken subMsg data.backend
                            in
                            ( { data | backend = backend }
                            , Cmd.map (MsgLoggedIn << MsgBackend) cmd
                            , List.map MsgCache cacheMsgs
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
                )
                model

        MsgLogin subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, cmd, loggedIn ) =
                            Restful.Login.update loginConfig subMsg configured.login

                        extraMsgs =
                            -- This will be true only at the very moment of
                            -- successful login.
                            if loggedIn == Just LoggedIn then
                                let
                                    -- Transition away from the `LoginPage` if
                                    -- that's where we are. We don't want to
                                    -- **prohibit** being on the LoginPage if
                                    -- you're already logged in, so we can't
                                    -- just detect the state of being logged in
                                    -- ... we have to get a message at the
                                    -- moment of login.
                                    redirect =
                                        case model.activePage of
                                            LoginPage ->
                                                case subModel of
                                                    Authenticated { credentials } ->
                                                        if EverySet.member Administrator credentials.user.roles then
                                                            -- We leave admins on the login page
                                                            []

                                                        else
                                                            [ SetActivePage <| UserPage <| ClinicsPage Nothing ]

                                                    _ ->
                                                        []

                                            _ ->
                                                -- If we were showing the LoginPage
                                                -- **instead of** the activePage, we
                                                -- can just actually show the activePage
                                                -- now
                                                []

                                    -- We also try to refetch the offlineSession.
                                    -- We actually do this in two places ...
                                    -- here, and when we get the session
                                    -- information from the cache.  Whichever
                                    -- gets triggered first will fail silently
                                    -- ...  the second will succeed.
                                    refetch =
                                        MsgSession <|
                                            Pages.Model.MsgEditableSession Backend.Session.Model.RefetchSession

                                    -- And, we reset errors, since the errors
                                    -- may have been authorization errors.
                                    resetErrors =
                                        MsgLoggedIn <| MsgBackend <| Backend.Model.ResetErrors
                                in
                                refetch :: resetErrors :: redirect

                            else
                                []
                    in
                    ( { configured | login = subModel }
                    , cmd
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
                                |> HttpBuilder.withExpectJson (Json.Decode.field "data" Device.Decoder.decode)
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

        MsgPageLogin subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, subCmd, outMsg ) =
                            Pages.Login.Update.update subMsg configured.loginPage

                        extraMsgs =
                            outMsg
                                |> Maybe.Extra.toList
                                |> List.map
                                    (\out ->
                                        case out of
                                            Pages.Login.Model.TryLogin name pass ->
                                                Restful.Login.tryLogin configured.config.backendUrl [] name pass
                                                    |> MsgLogin

                                            Pages.Login.Model.Logout ->
                                                MsgLogin Restful.Login.logout

                                            Pages.Login.Model.SetActivePage page ->
                                                SetActivePage page
                                    )
                    in
                    ( { configured | loginPage = subModel }
                    , Cmd.map MsgPageLogin subCmd
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

        MsgSession subMsg ->
            -- TODO: Should ideally reflect the fact that an EditableSession is
            -- required in the types.
            withEditableSession ( model, Cmd.none )
                (\_ session ->
                    let
                        ( subModel, subCmd, extraMsgs ) =
                            Pages.Update.updateSession session subMsg model.sessionPages
                    in
                    ( { model | sessionPages = subModel }
                    , Cmd.map MsgSession subCmd
                    )
                        |> sequence update extraMsgs
                )
                model.cache

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

        SetOffline offline ->
            ( { model | offline = offline }
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

TODO: Put a version of this in `Restful.Login`.

-}
updateLoggedIn : (Credentials User -> LoggedInModel -> ( LoggedInModel, Cmd Msg, List Msg )) -> Model -> ( Model, Cmd Msg )
updateLoggedIn func model =
    updateConfigured
        (\configured ->
            -- TODO: Perhaps we should Debug.log some errors in cases where we get
            -- messages we can't handle ...
            case configured.login of
                Anonymous _ ->
                    ( configured, Cmd.none, [] )

                Authenticated login ->
                    let
                        ( subModel, cmd, extraMsgs ) =
                            func login.credentials login.data
                    in
                    ( { configured | login = Authenticated { login | data = subModel } }
                    , cmd
                    , extraMsgs
                    )
        )
        model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every minute Tick
        , offline SetOffline
        , Sub.map MsgCache Backend.Update.subscriptions
        , Sub.map MsgServiceWorker ServiceWorker.Update.subscriptions
        ]


{-| Saves credentials provided by `Restful.Login`.
-}
port cacheCredentials : ( String, String ) -> Cmd msg


{-| Send Pusher key and cluster to JS.
-}
port pusherKey : ( String, String, List String ) -> Cmd msg


{-| Get a signal if internet connection is lost or regained.
-}
port offline : (Bool -> msg) -> Sub msg


{-| Set the user's current language.
-}
port setLanguage : String -> Cmd msg
