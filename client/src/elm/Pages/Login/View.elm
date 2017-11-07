module Pages.Login.View exposing (view)

import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Login.Model exposing (..)
import Pages.Page exposing (Page)
import Restful.Login exposing (LoginStatus(..))
import Translate exposing (translate, Language)
import User.Model exposing (..)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| TODO: We should have translated error messages specific to the login scenario.
I suppose the three real possible errors are:

  - credentials rejected ...
  - network error ...
  - internal error ...

Actually, I suppose this ought to be something that `Restful.Login` manages
(that is, converting `Http.Error` to more specific errors.

-}
view : Language -> Page -> LoginStatus User data -> Model -> Html Msg
view language activePage loginStatus model =
    -- We always show the wrapper and the logo. Then, we call `viewContent`
    -- to supply the rest, depending on our params.
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic login segment" ]
            (viewLogo language :: viewContent language activePage loginStatus model)
        ]


{-| Here, we differentiate based on whether we're logged in or not ... and
show something appropriate based on that.
-}
viewContent : Language -> Page -> LoginStatus User data -> Model -> List (Html Msg)
viewContent language activePage loginStatus model =
    case loginStatus of
        CheckingCachedCredentials ->
            [ viewCheckingCachedCredentials language ]

        Anonymous _ ->
            -- If we're here and we're anonymous, then we'll show the login
            -- form ... so, we'll see the fields and the form will take into
            -- account progress and error conditions.
            viewLoginForm language activePage loginStatus model

        LoggedIn login ->
            case login.relogin of
                Just progress ->
                    -- We have some login information, but we need to re-login
                    -- ...  our access token was rejected at some point. For
                    -- the moment, we'll just show the login form.
                    --
                    -- TODO: Should probably say something in the UI about
                    -- relogin ... i.e. distinguish this a little bit from a
                    -- "cold" login. Also, we might "fix" the username ...
                    -- that is, not allow the user to enter a different username
                    -- unless they actually logout first.
                    viewLoginForm language activePage loginStatus model

                Nothing ->
                    -- We're logged in, and, as far as we know, our access token
                    -- is still good.
                    [ viewLogout language login.credentials.user ]


{-| Some HTML to show while we're checking our cached credentials to see
if they are valid. We'll soon transition out of this state, one way or
another.

TODO: Think about what we want to show in this case ...

-}
viewCheckingCachedCredentials : Language -> Html any
viewCheckingCachedCredentials language =
    div []
        [ text <| translate language (Translate.LoginPhrase Translate.CheckingCachedCredentials)
        , spinner
        ]


{-| Show some HTML when we're actually logged in. Basically, allowing the
opportunity to logout.

TODO: Make this look nicer. Also, we should show some links to things you can
do ... i.e. the next stage of the app. (And, we could automatically transition
to the next stage, but that's not our job here, of course, since we're the
`view`).

-}
viewLogout : Language -> User -> Html Msg
viewLogout language user =
    div []
        [ p []
            [ Translate.LoginPhrase Translate.LoggedInAs
                |> translate language
                |> text
            , text <| ": " ++ user.name
            ]
        , button
            [ class "ui fluid primary button"
            , onClick HandleLogoutClicked
            ]
            [ Translate.LoginPhrase Translate.Logout
                |> translate language
                |> text
            ]
        ]


{-| Shows the login form itself, i.e. with inputs for username and password.
-}
viewLoginForm : Language -> Page -> LoginStatus User data -> Model -> List (Html Msg)
viewLoginForm language activePage loginStatus model =
    let
        -- A convenience for translating a `LoginPhrase`
        translateLogin =
            translate language << Translate.LoginPhrase

        isLoading =
            Restful.Login.isProgressing loginStatus

        disableSubmitButton =
            isLoading || model.name == "" || model.pass == ""

        activePageMsg =
            -- Show a little message if the user wanted to view a different page,
            -- but got sent here instead ...
            showIf (activePage /= Pages.Page.LoginPage) <|
                p []
                    [ text <| translateLogin Translate.YouMustLoginBefore
                    , text " "
                    , text <| translate language <| Translate.ActivePage activePage
                    , text " "
                    , text <| translate language Translate.Page
                    ]

        error =
            Restful.Login.getError loginStatus
                |> Maybe.map
                    (\err ->
                        div
                            [ class "ui error message" ]
                            [ viewError language err ]
                    )
                |> Maybe.withDefault emptyNode
    in
        [ activePageMsg
        , Html.form
            [ onSubmit HandleLoginClicked
            , action "javascript:void(0);"
            ]
            [ div
                [ class "ui login form" ]
                [ div
                    [ class "ui transparent left icon input" ]
                    [ input
                        [ placeholder <| translateLogin Translate.Username
                        , type_ "text"
                        , onInput SetName
                        , value model.name
                        , autofocus True
                        ]
                        []
                    , i [ class "icon icon-username" ] []
                    ]
                , div [ class "ui fitted divider" ] []
                , div
                    [ class "ui transparent left icon input" ]
                    [ input
                        [ placeholder <| translateLogin Translate.Password
                        , type_ "password"
                        , onInput SetPassword
                        , value model.pass
                        ]
                        []
                    , i [ class "icon icon-password" ] []
                    ]
                ]
            , button
                [ class "ui fluid primary button"
                , disabled disableSubmitButton
                , type_ "submit"
                ]
                [ span
                    [ hidden <| not isLoading ]
                    [ spinner ]
                , span
                    [ hidden isLoading ]
                    [ text <| translateLogin Translate.SignIn ]
                ]
            ]
        , error
        , p []
            [ text <| translateLogin Translate.ForgotPassword1
            , br [] []
            , text <| translateLogin Translate.ForgotPassword2
            ]
        ]


{-| Show the logo and name of the app.
-}
viewLogo : Language -> Html any
viewLogo language =
    let
        appName =
            translate language Translate.AppName
    in
        div
            [ class "logo" ]
            [ img
                [ alt appName
                , class "img-logo"
                , height 245
                , width 245
                , src "assets/images/logo-app.svg"
                ]
                []
            , br [] []
            , text appName
            ]
