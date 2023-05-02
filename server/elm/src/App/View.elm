module App.View exposing (view)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Error.View
import Html exposing (..)
import Pages.Scoreboard.View


view : Model -> Html Msg
view model =
    case model.activePage of
        Scoreboard ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgScoreboardPage <|
                    Pages.Scoreboard.View.view
                        model.language
                        model.scoreboardPage
                ]

        _ ->
            div []
                [ text "Wrong page?"
                ]
