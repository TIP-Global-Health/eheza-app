module Pages.ChildScoreboard.ProgressReport.View exposing (view)

import Backend.ChildScoreboardActivity.Utils exposing (allActivities)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.ChildScoreboard.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.ChildScoreboard.Encounter.View exposing (acuteIllnessEncounterPopup, viewEndEncounterButton)
import Pages.ChildScoreboard.ProgressReport.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChild.ProgressReport.View exposing (viewNCDAScorecard)
import SyncManager.Model exposing (Site)
import Translate exposing (translate)
import Translate.Model exposing (Language)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores site id db model =
    let
        assembled =
            generateAssembledData site id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> Site -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores site db model assembled =
    let
        ( _, pendingActivities ) =
            List.filter (expectActivity currentDate site assembled) allActivities
                |> List.partition (activityCompleted currentDate site assembled db)

        allowEndEncounter =
            List.isEmpty pendingActivities

        endEncounterButton =
            viewEndEncounterButton language assembled allowEndEncounter ShowAIEncounterPopup CloseEncounter
    in
    div [ class "page-report child-scoreboard" ]
        [ viewHeader language assembled.id
        , div [ class "ui report unstackable items" ] <|
            viewNCDAScorecard language currentDate zscores site ( assembled.participant.person, assembled.person ) db
                ++ [ endEncounterButton ]
        , viewModal <| acuteIllnessEncounterPopup language assembled model.showAIEncounterPopup TriggerAcuteIllnessEncounter
        ]


viewHeader : Language -> ChildScoreboardEncounterId -> Html Msg
viewHeader language id =
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.ChildScorecard ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage <| ChildScoreboardEncounterPage id)
            ]
            [ span [ class "icon-back" ] [] ]
        ]
