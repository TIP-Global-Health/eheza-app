module App.View exposing (view)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Error.View
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
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
                        model.backend
                        model.reportsMenuPage
                ]

        Reports ->
            let
                _ =
                    Debug.log "" <|
                        case model.backend.reportsData of
                            Just (Ok data) ->
                                List.length data.records

                            Just (Err err) ->
                                0

                            Nothing ->
                                0
            in
            -- @todo
            text "@todo"

        NotFound ->
            div []
                [ text "Wrong page?"
                ]
