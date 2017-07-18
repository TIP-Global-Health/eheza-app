module App.View exposing (..)

import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Config.View
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, src, style, target)
import Html.Events exposing (onClick)
import Pages.Login.View exposing (..)
import Pages.MyAccount.View exposing (..)
import Pages.PageNotFound.View exposing (..)
import PatientManager.View exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)
import Utils.Html exposing (emptyNode)


view : Model -> Html Msg
view model =
    case model.config of
        Failure err ->
            Config.View.view model.language

        Success config ->
            div [ class "wrap" ]
                [ viewSidebar model.language model
                , viewHeader model.language model
                , div [ class "ui main container" ]
                    [ viewMainContent config.backendUrl model
                    ]
                , div [ class "right item" ]
                    [ a
                        [ class "ui inverted button"
                        , id "sign-out"
                        , onClick Logout
                        ]
                        [ text <| translate model.language Trans.SignOut ]
                    ]
                ]

        _ ->
            emptyNode


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    case model.user of
        Success user ->
            div [ class "ui head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Trans.TitleHealthAssessment ]
                , a
                    [ class "link-back"
                    , href "#"
                    ]
                    [ span [ class "icon-back" ]
                        []
                    , span [] []
                    ]
                , viewHeaderThemeSwitcher model
                , viewTabSwitcher language model
                ]

        _ ->
            div [] []


{-| The theme switcher link.
-}
viewHeaderThemeSwitcher : Model -> Html Msg
viewHeaderThemeSwitcher model =
    a
        [ class "link-theme"
        , id "theme-switcher"
        , href "javascript:void(0);"
        , onClick (ThemeSwitch model.theme)
        ]
        [ span
            [ class "icon-theme icon-theme-light" ]
            []
        , span [] []
        ]


{-| Provides context-sensitive top navigation tabs.
There are two distinct contexts:

  - on the patient page
  - everywhere else

-}
viewTabSwitcher : Language -> Model -> Html Msg
viewTabSwitcher language model =
    let
        links =
            case model.activePage of
                Patient _ ->
                    [ a
                        [ classByPage (Dashboard []) model.activePage
                        ]
                        [ text <| translate language Trans.Mother ]
                    , a
                        [ classByPage (Activities) model.activePage
                        ]
                        [ text <| translate language Trans.Baby ]
                    ]

                _ ->
                    [ a
                        [ classByPage (Dashboard []) model.activePage
                        , onClick <| SetActivePage <| Dashboard []
                        ]
                        [ text <| translate language Trans.Patients ]
                    , a
                        [ classByPage Activities model.activePage
                        , onClick <| SetActivePage Activities
                        ]
                        [ text <| translate language Trans.Activities ]
                    ]
    in
        div [ class "ui fluid two item secondary pointing menu" ] links


viewSidebar : Language -> Model -> Html Msg
viewSidebar language model =
    case model.user of
        Success user ->
            div
                [ class "ui vertical inverted sidebar menu" ]
                [ a
                    [ class " header item"
                    , onClick <| SetActivePage MyAccount
                    ]
                    [ i
                        [ class "user icon" ]
                        []
                    , text user.name
                    ]
                , a
                    [ class "item"
                    , onClick <| SetActivePage Activities
                    ]
                    [ text <| translate language Trans.Activities ]
                , a
                    [ class "item"
                    , onClick <| SetActivePage <| Dashboard []
                    ]
                    [ text <| translate language Trans.Patients ]
                , a
                    [ class "item"
                    , onClick Logout
                    ]
                    [ text <| translate language Trans.SignOut ]
                , span
                    [ class "item"
                    ]
                    [ text <|
                        translate language <|
                            if model.offline then
                                Trans.NotConnected
                            else
                                Trans.Connected
                    , i
                        [ classList
                            [ ( "icon wifi", True )
                            , ( "disabled", model.offline )
                            ]
                        ]
                        []
                    ]
                ]

        _ ->
            div [] []


navbarAnonymous : Language -> Model -> List (Html Msg)
navbarAnonymous language model =
    [ a
        [ classByPage Login model.activePage
        , onClick <| SetActivePage Login
        ]
        [ text <| translate language Trans.Login ]
    , viewPageNotFoundPatient language model.activePage
    ]


navbarAuthenticated : Language -> Model -> List (Html Msg)
navbarAuthenticated language model =
    [ a
        [ classByPage MyAccount model.activePage
        , onClick <| SetActivePage MyAccount
        ]
        [ text <| translate language Trans.MyAccount ]
    , viewPageNotFoundPatient language model.activePage
    , div [ class "right menu" ]
        [ viewAvatar language model.user
        , a
            [ class "ui patient"
            , onClick <| Logout
            ]
            [ text <| translate language Trans.Logout ]
        ]
    ]


viewPageNotFoundPatient : Language -> Page -> Html Msg
viewPageNotFoundPatient language activePage =
    a
        [ classByPage PageNotFound activePage
        , onClick <| SetActivePage PageNotFound
        ]
        [ text <| translate language Trans.Page404 ]


viewAvatar : Language -> WebData User -> Html Msg
viewAvatar language user =
    case user of
        Success user_ ->
            a
                [ onClick <| SetActivePage MyAccount
                , class "ui patient"
                ]
                [ img
                    [ class "ui avatar image"
                    , src user_.avatarUrl
                    ]
                    []
                ]

        _ ->
            div [] []


viewMainContent : BackendUrl -> Model -> Html Msg
viewMainContent backendUrl model =
    let
        language =
            model.language

        viewContent =
            case model.activePage of
                AccessDenied ->
                    div [] [ text <| translate language Trans.AccessDenied ]

                Activities ->
                    case model.user of
                        Success user ->
                            Html.map MsgPatientManager <|
                                PatientManager.View.viewActivities model.language model.currentDate user model.pagePatient

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]

                Login ->
                    Html.map PageLogin (Pages.Login.View.view language model.user model.pageLogin)

                MyAccount ->
                    Pages.MyAccount.View.view language model.user

                PageNotFound ->
                    -- We don't need to pass any cmds, so we can call the view directly
                    Pages.PageNotFound.View.view language

                Dashboard _ ->
                    case model.user of
                        Success user ->
                            Html.map MsgPatientManager <|
                                PatientManager.View.viewPatients model.language model.currentDate user model.pagePatient

                        _ ->
                            div []
                                [ i [ class "notched circle loading icon" ] [] ]

                Patient id ->
                    case model.user of
                        Success user ->
                            Html.map MsgPatientManager <|
                                PatientManager.View.viewPagePatient backendUrl model.accessToken user model.language model.currentDate id model.pagePatient

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]
    in
        case model.user of
            NotAsked ->
                if String.isEmpty model.accessToken then
                    viewContent
                else
                    -- User might be logged in, so no need to present the login form.
                    -- So we first just show a throbber
                    div []
                        [ i [ class "icon loading spinner" ] []
                        ]

            _ ->
                viewContent


{-| Get menu items classes. This function gets the active page and checks if
it is indeed the page used.
-}
classByPage : Page -> Page -> Attribute a
classByPage page activePage =
    classList
        [ ( "item", True )
        , ( "active", page == activePage )
        ]
