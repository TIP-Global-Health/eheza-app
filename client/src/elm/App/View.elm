module App.View exposing (view)

import App.Model exposing (..)
import Backend.Model exposing (CachedSessionError(..))
import Config.View
import Date
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Pages.Admin.View
import Pages.Clinics.View
import Pages.Device.View
import Pages.Login.View
import Pages.MyAccount.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.PinCode.View
import Pages.View exposing (viewFoundSession)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Login as RL
import ServiceWorker.View
import Translate exposing (Language(..), translate)
import User.Model exposing (User)
import Utils.Html exposing (spinner, wrapPage)
import Version


view : Model -> Html Msg
view model =
    case model.configuration of
        Failure err ->
            Config.View.view model.language

        Success configuration ->
            div [ class "container" ]
                [ viewLanguageSwitcherAndVersion model

                -- We supply the model as well as the resolved configuration ...
                -- it's easier that way.
                , viewConfiguredModel model configuration
                ]

        _ ->
            -- Don't show anything if config resolution is in process but
            -- hasn't failed yet.
            viewLoading


{-| The language switcher view which sets a preferred language for each user and
saves the current language via the Update function in local storage.
-}
viewLanguageSwitcherAndVersion : Model -> Html Msg
viewLanguageSwitcherAndVersion model =
    div
        [ class "ui language-switcher" ]
        [ ul
            [ class "links-translate" ]
            [ li
                [ classList
                    [ ( "item english", True )
                    , ( "active", model.language == English )
                    ]
                , onClick <| SetLanguage English
                ]
                [ text "English"
                , a [] [ span [ class "icon-english" ] [] ]
                ]
            , li
                [ classList
                    [ ( "item kinyarwanda", True )
                    , ( "active", model.language == Kinyarwanda )
                    ]
                , onClick <| SetLanguage Kinyarwanda
                ]
                [ text "Kinyarwanda"
                , a [] [ span [ class "icon-kinyarwanda" ] [] ]
                ]
            ]
        , span
            [ class "version"
            , onClick <| SetActivePage ServiceWorkerPage
            ]
            [ ServiceWorker.View.viewIcon model.serviceWorker
            , text <| translate model.language Translate.Version
            , text ": "
            , text <| .build Version.version
            ]
        ]


{-| We call this after checking our config. We ask for the model itself,
because we'll need several things from it and it's easier than asking for those
things as separate params. We ask for the `ConfiguredModel` as a guarantee that
we have one. That is, it's the caller's job to do something sensible if we
don't have one.
-}
viewConfiguredModel : Model -> ConfiguredModel -> Html Msg
viewConfiguredModel model configured =
    if not model.serviceWorker.active then
        -- If our service worker is not active, then the only thing we allow
        -- is showing the status of the service worker. (Since we need the
        -- service worker for the normal operation of the app).
        ServiceWorker.View.view model.currentTime model.language model.serviceWorker
            |> Html.map MsgServiceWorker

    else if not (RemoteData.isSuccess configured.device) then
        -- If our device is not paired, then the only thing we allow is the pairing
        -- of the device
        Pages.Device.View.view model.language configured.device model configured.devicePage
            |> Html.map MsgPageDevice

    else
        case model.activePage of
            DevicePage ->
                Pages.Device.View.view model.language configured.device model configured.devicePage
                    |> Html.map MsgPageDevice

            LoginPage ->
                viewEditableSession model configured

            PinCodePage ->
                Pages.PinCode.View.view model.language model.activePage model.nurse model.pinCodePage
                    |> Html.map MsgPagePinCode

            PageNotFound url ->
                Pages.PageNotFound.View.view model.language url

            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker

            SessionPage subPage ->
                viewEditableSession model configured

            UserPage userPage ->
                viewEditableSession model configured


viewEditableSession : Model -> ConfiguredModel -> Html Msg
viewEditableSession model configured =
    let
        currentDate =
            fromLocalDateTime <| Date.fromTime model.currentTime
    in
    -- What we are able to do here depends on whether we've logged in or not.
    -- So, in effect, this is where we're doing what you would think of as
    -- "access control". And, we also want to wait until we know whether
    -- we have an editable session or not.
    case model.cache.editableSession of
        Success session ->
            case configured.login of
                RL.Anonymous { progress } ->
                    case progress of
                        Just (RL.Checking _) ->
                            -- If we're checking cached credentials, show the login page ...
                            -- that's the logical place for some UI related to this.
                            Pages.Login.View.view model.language model.activePage configured.login configured.loginPage (Maybe.map Tuple.second session)
                                |> Html.map MsgPageLogin

                        _ ->
                            case model.activePage of
                                PageNotFound url ->
                                    Pages.PageNotFound.View.view model.language url

                                ServiceWorkerPage ->
                                    ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                                        |> Html.map MsgServiceWorker

                                _ ->
                                    Pages.Login.View.view model.language model.activePage configured.login configured.loginPage (Maybe.map Tuple.second session)
                                        |> Html.map MsgPageLogin

                RL.Authenticated login ->
                    -- If we're logged in, then we consult the `activePage` to
                    -- determine what the user wants to see. Note that this will
                    -- magically do a "redirect" to the user's desired page once the
                    -- login process finishes, since we don't change the activePage to
                    -- the login page ... we just show it when login is required.
                    --
                    -- Note that we're not yet consulting `login.relogin` to see
                    -- whether relogin is required. That would need to be not entirely
                    -- automatic, since we want to let the user keep working locally
                    -- until they are able to relogin.
                    case model.activePage of
                        DevicePage ->
                            Pages.Device.View.view model.language configured.device model configured.devicePage
                                |> Html.map MsgPageDevice

                        LoginPage ->
                            -- The user is already logged in, but wants to see the
                            -- login page. This is basically sensible ... we could put
                            -- a `Logout` button there, or we could do the `relogin`
                            -- process if that's needed. Or just report the login
                            -- status.
                            Pages.Login.View.view model.language model.activePage configured.login configured.loginPage (Maybe.map Tuple.second session)
                                |> Html.map MsgPageLogin

                        PinCodePage ->
                            Pages.PinCode.View.view model.language model.activePage model.nurse model.pinCodePage
                                |> Html.map MsgPagePinCode

                        UserPage userPage ->
                            case userPage of
                                AdminPage ->
                                    Pages.Admin.View.view configured.config model.language currentDate login.credentials.user login.data.backend login.data.adminPage
                                        |> Html.map (MsgLoggedIn << MsgPageAdmin)

                                MyAccountPage ->
                                    Pages.MyAccount.View.view model.language login.credentials.user

                                ClinicsPage clinicId ->
                                    Pages.Clinics.View.view model.language currentDate login.credentials.user clinicId login.data.backend model.cache

                        ServiceWorkerPage ->
                            ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                                |> Html.map MsgServiceWorker

                        PageNotFound url ->
                            Pages.PageNotFound.View.view model.language url

                        SessionPage subPage ->
                            viewSessionPage login.credentials.user subPage model

        Failure err ->
            viewCachedSessionError model.language err

        _ ->
            viewLoading


{-| Just show a generic loading indicator, for cases that will resolve soon,
where we don't need to show any progress.
-}
viewLoading : Html any
viewLoading =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui segment center aligned" ]
            [ spinner ]
        ]


viewSessionPage : User -> SessionPage -> Model -> Html Msg
viewSessionPage user page model =
    case model.cache.editableSession of
        NotAsked ->
            wrapPage [ spinner ]

        Loading ->
            wrapPage [ spinner ]

        Failure err ->
            viewCachedSessionError model.language err

        Success fetched ->
            case fetched of
                Just session ->
                    viewFoundSession user page session model
                        |> Html.map MsgSession

                Nothing ->
                    -- We have to handle this out here, because `MsgSession` messages don't
                    -- get processed at all if there is no `EditableSession`. So, the redirect
                    -- doesn't actually work if we do it in `viewFoundSession`.
                    wrapPage
                        [ div
                            [ class "ui error" ]
                            [ p [] [ text <| translate model.language Translate.NoCachedSession ]
                            , button
                                [ class "ui fluid primary button"
                                , onClick <| SetActivePage <| UserPage <| ClinicsPage Nothing
                                ]
                                [ text <| translate model.language Translate.SelectYourClinic ]
                            ]
                        ]


viewCachedSessionError : Language -> CachedSessionError -> Html any
viewCachedSessionError language err =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.ErrorFetchingCachedSessionTitle ]
            ]
        , div
            [ class "ui basic segment" ]
            [ p []
                [ text <| translate language Translate.ErrorFetchingCachedSessionMessage ]
            ]
        ]
