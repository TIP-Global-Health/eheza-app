module App.View exposing (view)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInModel)
import Config.View
import Date
import EveryDict
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
import Pages.ParticipantRegistration.View
import Pages.People.View
import Pages.Person.View
import Pages.PinCode.View
import Pages.Relationship.View
import Pages.Session.Model
import Pages.Session.View exposing (view)
import RemoteData exposing (RemoteData(..), WebData)
import ServiceWorker.View
import Translate exposing (translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (spinner, wrapPage)
import Version


view : Model -> Html Msg
view model =
    case model.configuration of
        Failure err ->
            Config.View.view model.language

        Success configuration ->
            div [ class "page container" ]
                [ viewLanguageSwitcherAndVersion model
                , div
                    [ class "page-content" ]
                    [ viewConfiguredModel model configuration ]
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
                    Pages.MyAccount.View.view model.language loggedInModel.nurse

                ClinicsPage clinicId ->
                    Pages.Clinics.View.view model.language currentDate (Tuple.second loggedInModel.nurse) clinicId model.indexedDb

                CreatePersonPage relation ->
                    Pages.Person.View.viewCreateForm model.language currentDate relation loggedInModel.createPersonPage model.indexedDb.postPerson
                        |> Html.map (MsgLoggedIn << MsgPageCreatePerson)

                PersonPage id ->
                    Pages.Person.View.view model.language currentDate id model.indexedDb

                PersonsPage search relation ->
                    Pages.People.View.view model.language currentDate search relation model.indexedDb

                ParticipantRegistrationPage ->
                    Pages.ParticipantRegistration.View.view model.language currentDate model.indexedDb loggedInModel.participantRegistrationPage
                        |> Html.map (MsgLoggedIn << MsgPageParticipantRegistration)

                RelationshipPage id1 id2 ->
                    Pages.Relationship.View.view model.language id1 id2 model.indexedDb

                SessionPage sessionId subPage ->
                    let
                        sessionPages =
                            EveryDict.get sessionId loggedInModel.sessionPages
                                |> Maybe.withDefault Pages.Session.Model.emptyModel
                    in
                    Pages.Session.View.view
                        model.language
                        currentDate
                        model.zscores
                        (Tuple.second loggedInModel.nurse)
                        sessionId
                        subPage
                        sessionPages
                        model.indexedDb
                        |> Html.map (MsgLoggedIn << MsgPageSession sessionId)

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
