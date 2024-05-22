module Pages.ScoreboardMenu.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Backend.ScoreboardMenu.Model exposing (MenuData)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust)
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton)
import Pages.ScoreboardMenu.Model exposing (..)
import Pages.Utils exposing (viewCustomLabel, viewGeoLocationSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


view : Language -> ModelBackend -> Model -> Html Msg
view language modelBackend model =
    case modelBackend.scoreboardMenuData of
        Just (Ok data) ->
            viewMenu language data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewMenu : Language -> MenuData -> Model -> Html Msg
viewMenu language data model =
    let
        geoInfo =
            getGeoInfo data.site

        ( inputs, actionButton_ ) =
            ( viewDemographicsSelection language data.site SetGeoLocation model.selectedDemographics
            , if isJust model.selectedDemographics.province && isJust model.selectedDemographics.district then
                viewDemographicsSelectionActionButton language
                    data.site
                    "/admin/reports/aggregated-ncda"
                    SelectionMade
                    model.selectedDemographics

              else
                emptyNode
            )

        actionButton =
            if model.selected then
                text <| translate language Translate.PleaseWaitMessage

            else
                actionButton_
    in
    div [ class "page-content" ]
        [ viewCustomLabel language Translate.SelectViewMode ":" "header"
        , div [ class "inputs" ] inputs
        , div [ class "actions" ] [ actionButton ]
        ]
