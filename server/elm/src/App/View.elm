module App.View exposing (view)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Error.View
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Pages.Reports.View
import Pages.ReportsMenu.View
import Pages.Scoreboard.View
import Pages.ScoreboardMenu.View


view : Model -> Html Msg
view model =
    case model.activePage of
        ScoreboardMenu ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgScoreboardMenuPage <|
                    Pages.ScoreboardMenu.View.view
                        model.language
                        model.backend
                        model.scoreboardMenuPage
                ]

        Scoreboard ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgScoreboardPage <|
                    Pages.Scoreboard.View.view
                        model.language
                        (fromLocalDateTime model.currentTime)
                        model.backend
                        model.scoreboardPage
                ]

        ReportsMenu ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgReportsMenuPage <|
                    Pages.ReportsMenu.View.view
                        model.language
                        model.themePath
                        model.backend
                        model.reportsMenuPage
                ]

        Reports ->
            div []
                [ Error.View.view model.language model.errors
                , Html.map MsgReportsPage <|
                    Pages.Reports.View.view
                        model.language
                        (fromLocalDateTime model.currentTime)
                        model.themePath
                        model.backend
                        model.reportsPage
                ]

        NotFound ->
            div []
                [ text "Wrong page?" ]
