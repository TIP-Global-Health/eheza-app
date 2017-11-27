module App.View exposing (view)

import App.Model exposing (..)
import Config.View
import Html exposing (..)
import Html.Attributes exposing (class)
import Pages.Clinics.View
import Pages.Login.View
import Pages.MyAccount.View
import Pages.PageNotFound.View
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.View exposing (viewSessionPage)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Login
import Utils.Html exposing (spinner)


view : Model -> Html Msg
view model =
    case model.configuration of
        Failure err ->
            Config.View.view model.language

        Success configuration ->
            -- We supply the model as well as the resolved configuration ...
            -- it's easier that way.
            viewConfiguredModel model configuration

        _ ->
            -- Don't show anything if config resolution is in process but
            -- hasn't failed yet.
            viewLoading


{-| We call this after checking our config. We ask for the model itself,
because we'll need several things from it and it's easier than asking for those
things as separate params. We ask for the `ConfiguredModel` as a guarantee that
we have one. That is, it's the caller's job to do something sensible if we
don't have one.
-}
viewConfiguredModel : Model -> ConfiguredModel -> Html Msg
viewConfiguredModel model configured =
    -- What we are able to do here depends on whether we've logged in or not.
    -- So, in effect, this is where we're doing what you would think of as
    -- "access control".
    case configured.login of
        Restful.Login.CheckingCachedCredentials ->
            -- If we're checking cached credentials, show the login page ...
            -- that's the logical place for some UI related to this.
            Pages.Login.View.view model.language model.activePage configured.login configured.loginPage
                |> Html.map MsgPageLogin

        Restful.Login.Anonymous progress ->
            -- An anonymous person can either login or, if we have a cached
            -- session, they can work on the cached session.
            --
            -- So, we mostly just show the login page. We supply the
            -- `activePage` so that the login page can possibly show some
            -- message about the page the user really wanted, if it likes.
            --
            -- Actually, I suppose another Page we ought to treat specially for
            -- aonymous users is `PageNotFound`, since it would be weird to
            -- successfully log in, only then to be taken to a page not found.
            case model.activePage of
                PageNotFound url ->
                    Pages.PageNotFound.View.view model.language url

                SessionPage subPage ->
                    Pages.View.viewSessionPage model.language model.currentDate subPage model.cache.editableSession model.sessionPages
                        |> Html.map MsgSession

                _ ->
                    Pages.Login.View.view model.language model.activePage configured.login configured.loginPage
                        |> Html.map MsgPageLogin

        Restful.Login.LoggedIn login ->
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
                LoginPage ->
                    -- The user is already logged in, but wants to see the
                    -- login page. This is basically sensible ... we could put
                    -- a `Logout` button there, or we could do the `relogin`
                    -- process if that's needed. Or just report the login
                    -- status.
                    Pages.Login.View.view model.language model.activePage configured.login configured.loginPage
                        |> Html.map MsgPageLogin

                UserPage userPage ->
                    case userPage of
                        MyAccountPage ->
                            Pages.MyAccount.View.view model.language login.credentials.user

                        ClinicsPage clinicId ->
                            -- We need to know whether we have a cached session
                            case model.cache.editableSession of
                                Success session ->
                                    Pages.Clinics.View.view model.language model.currentDate login.credentials.user clinicId login.data.backend session

                                _ ->
                                    -- TODO: Whose job should it be to show errors loading the offlineSession
                                    -- from the cache?
                                    viewLoading

                PageNotFound url ->
                    Pages.PageNotFound.View.view model.language url

                SessionPage subPage ->
                    Pages.View.viewSessionPage
                        model.language
                        model.currentDate
                        subPage
                        model.cache.editableSession
                        model.sessionPages
                        |> Html.map MsgSession


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
