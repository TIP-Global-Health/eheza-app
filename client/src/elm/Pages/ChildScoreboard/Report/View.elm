module Pages.ChildScoreboard.Report.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.ChildScoreboard.Report.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChild.ProgressReport.View exposing (viewNCDAScorecard)
import Translate exposing (Language, TranslationId, translate, translateText)
import Translate.Model exposing (Language(..))
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores db model assembled =
    div [ class "page-report child-scoreboard" ] <|
        viewHeader language assembled.id
            :: viewNCDAScorecard language currentDate zscores ( assembled.participant.person, assembled.person ) db


viewHeader : Language -> ChildScoreboardEncounterId -> Html Msg
viewHeader language id =
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.Scorecard ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage <| ChildScoreboardEncounterPage id)
            ]
            [ span [ class "icon-back" ] [] ]
        ]
