module Pages.Login.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Login.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (..)
import Utils.WebData exposing (viewError)


view : Language -> WebData User -> Model -> Html Msg
view language user model =
    div [ class "login-container" ]
        [ viewHeader language model
        , viewMain language user model
        ]


viewHeader : Language -> Model -> Html Msg
viewHeader language model =
    Html.header []
        [ a [ id "logo", href "/" ]
            [ img [ src "logo.png", alt "Logo" ] []
            ]
        ]


viewMain : Language -> WebData User -> Model -> Html Msg
viewMain language user model =
    let
        spinner =
            i [ class "notched circle loading icon" ] []

        ( isLoading, isError ) =
            case user of
                Loading ->
                    ( True, False )

                Failure _ ->
                    ( False, True )

                _ ->
                    ( False, False )

        error =
            case user of
                Failure err ->
                    div [ class "ui error message" ] [ viewError language err ]

                _ ->
                    div [] []
    in
        Html.main_ []
            [ Html.form
                [ onSubmit TryLogin
                , action "javascript:void(0);"
                , class "ui large form narrow-form"
                ]
                [ div [ class "field" ]
                    [ input
                        [ type_ "text"
                        , name "username"
                        , placeholder "Username"
                        , onInput SetName
                        , value model.loginForm.name
                        ]
                        []
                    ]
                , div [ class "field" ]
                    [ input
                        [ type_ "password"
                        , placeholder "Password"
                        , name "password"
                        , onInput SetPassword
                        , value model.loginForm.pass
                        ]
                        []
                    ]

                -- Submit button
                , button
                    [ disabled isLoading
                    , class "ui large fluid primary button"
                    ]
                    [ span [ hidden <| not isLoading ] [ spinner ]
                    , span [ hidden isLoading ] [ text <| translate language Trans.Login ]
                    ]
                ]
            , error
            ]
