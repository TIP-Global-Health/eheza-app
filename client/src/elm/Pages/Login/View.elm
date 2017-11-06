module Pages.Login.View exposing (view)

import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Login.Model exposing (..)
import Pages.Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Login exposing (LoginStatus(..))
import Translate as Trans exposing (translate, Language)
import User.Model exposing (..)
import Utils.WebData exposing (viewError)


{-| TODO: The `activePage` indicates what page the user really wanted ... we
may have shown this instead of that page, and we might indicate something
about that in the UI (e.g. "You must log in before view the ...").

Also, note that we're not taking `relogin` into account here yet, and we
should probably show something different if the user is logged in
(perhaps an opportunity to log out).

-}
view : Language -> Page -> LoginStatus user data -> Model -> Html Msg
view language activePage status model =
    div [ class "login-container" ]
        [ viewHeader
        , viewMain language activePage status model
        ]


viewHeader : Html Msg
viewHeader =
    Html.header []
        [ a [ id "logo" ]
            [ img [ src "assets/images/logo.png", alt "Logo" ] []
            ]
        ]


viewMain : Language -> Page -> LoginStatus user data -> Model -> Html Msg
viewMain language activePage loginStatus model =
    let
        spinner =
            i [ class "notched circle loading icon" ] []

        isLoading =
            Restful.Login.isProgressing loginStatus

        error =
            -- TODO: Should have more specific, translated error messages.
            Restful.Login.getError loginStatus
                |> Maybe.map
                    (\err ->
                        div
                            [ class "ui error message" ]
                            [ viewError language err ]
                    )
                |> Maybe.withDefault emptyNode
    in
        Html.main_ []
            [ Html.form
                [ onSubmit HandleLoginClicked
                , action "javascript:void(0);"
                , class "ui large form narrow-form login-form"
                ]
                [ div [ class "field" ]
                    [ input
                        [ type_ "text"
                        , name "username"
                        , placeholder <| translate language Trans.Username
                        , onInput SetName
                        , value model.loginForm.name
                        ]
                        []
                    ]
                , div [ class "field" ]
                    [ input
                        [ type_ "password"
                        , placeholder <| translate language Trans.Password
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
