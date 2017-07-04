module App.View exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Config.View
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, src, style, target)
import Html.Events exposing (onClick)
import User.Model exposing (..)
import Pages.Login.View exposing (..)
import Pages.MyAccount.View exposing (..)
import Pages.PageNotFound.View exposing (..)
import PatientManager.View exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (translate, Language)


view : Model -> Html Msg
view model =
    case model.config of
        Failure err ->
            Config.View.view model.language

        _ ->
            div []
                [ viewSidebar model.language model
                , div [ class "pusher" ]
                    [ viewHeader model.language model
                    , div [ class "ui main container" ]
                        [ viewMainContent model.language model
                        ]
                    ]
                ]


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    case model.user of
        Success user ->
            div [ class "ui inverted masthead segment" ]
                [ div [ class "ui container" ]
                    [ div [ class "ui large inverted secondary pointing menu" ]
                        [ a
                            [ class "toc item"
                            , href "#"
                            ]
                            [ i [ class "sidebar icon" ]
                                []
                            ]
                        , a
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
                            [ text "Activities" ]
                        , a
                            [ class "item"
                            , onClick <| SetActivePage <| Dashboard []
                            ]
                            [ text "Patients" ]
                        , div [ class "right item" ]
                            [ a
                                [ class "ui inverted button"
                                , onClick Logout
                                ]
                                [ text "Sign Out" ]
                            ]
                        , span
                            [ class "item"
                            ]
                            [ i
                                [ classList
                                    [ ( "icon wifi", True )
                                    , ( "disabled", model.offline )
                                    ]
                                ]
                                []
                            ]
                        ]
                    ]
                ]

        _ ->
            div [] []


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
                    [ text "Activities" ]
                , a
                    [ class "item"
                    , onClick <| SetActivePage <| Dashboard []
                    ]
                    [ text "Patients" ]
                , a
                    [ class "item"
                    , onClick Logout
                    ]
                    [ text "Sign Out" ]
                , span
                    [ class "item"
                    ]
                    [ text <|
                        if model.offline then
                            "Not Connected"
                        else
                            "Connected"
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
        [ text "Login" ]
    , viewPageNotFoundPatient language model.activePage
    ]


navbarAuthenticated : Language -> Model -> List (Html Msg)
navbarAuthenticated language model =
    [ a
        [ classByPage MyAccount model.activePage
        , onClick <| SetActivePage MyAccount
        ]
        [ text "My Account" ]
    , viewPageNotFoundPatient language model.activePage
    , div [ class "right menu" ]
        [ viewAvatar language model.user
        , a
            [ class "ui patient"
            , onClick <| Logout
            ]
            [ text "Logout" ]
        ]
    ]


viewPageNotFoundPatient : Language -> Page -> Html Msg
viewPageNotFoundPatient language activePage =
    a
        [ classByPage PageNotFound activePage
        , onClick <| SetActivePage PageNotFound
        ]
        [ text "404 page" ]


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


viewMainContent : Language -> Model -> Html Msg
viewMainContent language model =
    let
        viewContent =
            case model.activePage of
                AccessDenied ->
                    div [] [ text "Access denied" ]

                Activities ->
                    case model.user of
                        Success user ->
                            Html.map MsgPatientManager <|
                                PatientManager.View.viewActivities model.language model.currentDate user model.pagePatient

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]

                Login ->
                    Html.map PageLogin (Pages.Login.View.view model.user model.pageLogin)

                MyAccount ->
                    Pages.MyAccount.View.view model.user

                PageNotFound ->
                    -- We don't need to pass any cmds, so we can call the view directly
                    Pages.PageNotFound.View.view

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
                                PatientManager.View.viewPagePatient model.language model.currentDate id user model.pagePatient

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


{-| Get menu patients classes. This function gets the active page and checks if
it is indeed the page used.
-}
classByPage : Page -> Page -> Attribute a
classByPage page activePage =
    classList
        [ ( "patient", True )
        , ( "active", page == activePage )
        ]
