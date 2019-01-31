module App.View exposing (view)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInModel)
import Backend.Model exposing (CachedSessionError(..))
import Config.View
import Date
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Pages.Admin.View
import Pages.Clinics.View
import Pages.Device.View
import Pages.MyAccount.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.PinCode.View
import Pages.View exposing (viewFoundSession)
import RemoteData exposing (RemoteData(..), WebData)
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

            PinCodePage ->
                Pages.PinCode.View.view model.language model.activePage (RemoteData.map .nurse configured.loggedIn) configured.pinCodePage
                    |> Html.map MsgPagePinCode

            PageNotFound url ->
                Pages.PageNotFound.View.view model.language url

            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker

            UserPage userPage ->
                viewUserPage userPage model configured


viewUserPage : UserPage -> Model -> ConfiguredModel -> Html Msg
viewUserPage page model configured =
    let
        currentDate =
            fromLocalDateTime <| Date.fromTime model.currentTime
    in
    case getLoggedInModel model of
        Just loggedInModel ->
            case page of
                AdminPage ->
                    emptyNode

                -- TODO: Re-implement
                -- Pages.Admin.View.view configured.config model.language currentDate login.credentials.user login.data.backend login.data.adminPage
                --     |> Html.map (MsgLoggedIn << MsgPageAdmin)
                MyAccountPage ->
                    emptyNode

                -- TODO: Re-implement
                -- Pages.MyAccount.View.view model.language login.credentials.user
                ClinicsPage clinicId ->
                    Pages.Clinics.View.view model.language currentDate (Tuple.second loggedInModel.nurse) clinicId model.indexedDb

                SessionPage subPage ->
                    -- TODO: Re-implement
                    emptyNode

        Nothing ->
            Pages.PinCode.View.view model.language model.activePage (RemoteData.map .nurse configured.loggedIn) configured.pinCodePage
                |> Html.map MsgPagePinCode


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
