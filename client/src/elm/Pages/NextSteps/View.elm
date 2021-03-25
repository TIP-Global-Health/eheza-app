module Pages.NextSteps.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..), emptySummaryByActivity)
import Activity.Utils exposing (getActivityIcon, getAllActivities, getParticipantCountForActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.NutritionActivity.View exposing (warningPopup)
import Pages.NutritionEncounter.Model exposing (NutritionAssesment)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (backFromSessionPage, viewEndEncounterDialog)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (tabItem, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Activity -> ( SessionId, EditableSession ) -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores childId originActivity ( sessionId, session ) db model =
    let
        header =
            viewHeader language

        content =
            Dict.get childId db.people
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (viewContent language currentDate zscores childId)
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-activity nutrition" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.NextSteps ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Html Msg
viewContent language currentDate zscores childId child =
    ((viewPersonDetails language currentDate child Nothing |> div [ class "item" ])
        :: []
     -- viewActivity language currentDate zscores id activity isChw assembled db model
    )
        |> div [ class "ui unstackable items" ]
