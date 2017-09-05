module App.View exposing (..)

import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Config.View
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, src, style, target)
import Html.Events exposing (onClick)
import Pages.Login.View exposing (..)
import Pages.MyAccount.View exposing (..)
import Pages.PageNotFound.View exposing (..)
import ParticipantManager.View exposing (..)
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
            case model.activePage of
                Activities ->
                    case model.user of
                        Success user ->
                            Html.map MsgParticipantManager <|
                                ParticipantManager.View.viewActivities model.language model.pageParticipant

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]

                Dashboard _ ->
                    case model.user of
                        Success user ->
                            Html.map MsgParticipantManager <|
                                ParticipantManager.View.viewParticipants model.language model.pageParticipant

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]

                Participant id ->
                    case model.user of
                        Success user ->
                            Html.map MsgParticipantManager <|
                                ParticipantManager.View.viewPageParticipant model.language model.currentDate id model.pageParticipant

                        _ ->
                            div [] [ i [ class "notched circle loading icon" ] [] ]

                _ ->
                    div [ class "wrap" ]
                        [ viewSidebar model.language model
                        , viewHeader model.language model
                        , div [ class "ui main container" ]
                            [ viewMainContent model
                            ]
                        ]

        _ ->
            emptyNode


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    case model.user of
        Success user ->
            case model.activePage of
                Activities ->
                    emptyNode

                Dashboard _ ->
                    emptyNode

                Participant _ ->
                    emptyNode

                _ ->
                    div [ class "ui head segment" ]
                        [ h1
                            [ class "ui header" ]
                            [ text <| translate language Trans.TitleHealthAssessment ]
                        , viewHeaderBackButton model
                        , viewTabSwitcher language model
                        ]

        _ ->
            div [] []


{-| The back button link.
-}
viewHeaderBackButton : Model -> Html Msg
viewHeaderBackButton model =
    a
        [ class "link-back"
        , onClick <| RedirectByActivePage
        ]
        [ span
            [ class "icon-back" ]
            []
        ]


{-| Provides context-sensitive top navigation tabs.
There are two distinct contexts:

  - on the participant page
  - everywhere else

-}
viewTabSwitcher : Language -> Model -> Html Msg
viewTabSwitcher language model =
    div [ class "ui fluid two item secondary pointing menu" ]
        [ a
            [ classByPage (Dashboard []) model.activePage
            , onClick <| SetActivePage <| Dashboard []
            ]
            [ text <| translate language Trans.Participants ]
        , a
            [ classByPage Activities model.activePage
            , onClick <| SetActivePage Activities
            ]
            [ text <| translate language Trans.Activities ]
        ]


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
                    [ text <| translate language Trans.Participants ]
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
    , viewPageNotFoundParticipant language model.activePage
    ]


navbarAuthenticated : Language -> Model -> List (Html Msg)
navbarAuthenticated language model =
    [ a
        [ classByPage MyAccount model.activePage
        , onClick <| SetActivePage MyAccount
        ]
        [ text <| translate language Trans.MyAccount ]
    , viewPageNotFoundParticipant language model.activePage
    , div [ class "right menu" ]
        [ viewAvatar language model.user
        , a
            [ class "ui participant"
            , onClick <| Logout
            ]
            [ text <| translate language Trans.Logout ]
        ]
    ]


viewPageNotFoundParticipant : Language -> Page -> Html Msg
viewPageNotFoundParticipant language activePage =
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
                , class "ui participant"
                ]
                [ img
                    [ class "ui avatar image"
                    , src user_.avatarUrl
                    ]
                    []
                ]

        _ ->
            div [] []


viewMainContent : Model -> Html Msg
viewMainContent model =
    let
        language =
            model.language

        viewContent =
            case model.activePage of
                AccessDenied ->
                    div [] [ text <| translate language Trans.AccessDenied ]

                Activities ->
                    emptyNode

                Login ->
                    Html.map PageLogin (Pages.Login.View.view language model.user model.pageLogin)

                MyAccount ->
                    Pages.MyAccount.View.view language model.user

                PageNotFound ->
                    -- We don't need to pass any cmds, so we can call the view directly
                    Pages.PageNotFound.View.view language

                Dashboard _ ->
                    emptyNode

                Participant id ->
                    emptyNode
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
