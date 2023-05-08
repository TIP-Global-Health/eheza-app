module App.View exposing (view)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Error.View
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Pages.Menu.View
import Pages.Scoreboard.View


view : Model -> Html Msg
view model =
    case model.activePage of
        Menu ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgMenuPage <|
                    Pages.Menu.View.view
                        model.language
                        model.menuPage
                ]

        Scoreboard ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgScoreboardPage <|
                    Pages.Scoreboard.View.view
                        model.language
                        (fromLocalDateTime model.currentTime)
                        model.scoreboardPage
                ]

        _ ->
            div []
                [ text "Wrong page?"
                ]
