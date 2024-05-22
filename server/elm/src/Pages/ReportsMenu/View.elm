module Pages.ReportsMenu.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Backend.ReportsMenu.Model exposing (MenuData)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust)
import Pages.Components.Model exposing (DemographicsSelection)
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton)
import Pages.ReportsMenu.Model exposing (..)
import Pages.ReportsMenu.Types exposing (..)
import Pages.ReportsMenu.Utils exposing (populationSelectionOptionToString)
import Pages.Utils exposing (viewCustomLabel, viewCustomSelectListInput, viewGeoLocationSelectListInput, viewSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


view : Language -> ModelBackend -> Model -> Html Msg
view language modelBackend model =
    case modelBackend.reportsMenuData of
        Just (Ok data) ->
            viewMenu language data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewMenu : Language -> MenuData -> Model -> Html Msg
viewMenu language data model =
    let
        populationSelectionInput =
            let
                options =
                    [ SelectionOptionGlobal, SelectionOptionDemographics, SelectionOptionHealthCenter ]
            in
            viewSelectListInput language
                model.populationSelection
                options
                populationSelectionOptionToString
                SetPopulationSelection
                Translate.PopulationSelectionOption
                "select"

        ( derivedInputs, actionButton_ ) =
            Maybe.map
                (\populationSelection ->
                    case populationSelection of
                        SelectionOptionGlobal ->
                            ( [], emptyNode )

                        SelectionOptionDemographics ->
                            ( [ viewDemographicsSelection language data SetGeoLocation model.selectedDemographics ]
                            , if isJust model.selectedDemographics.province then
                                viewDemographicsSelectionActionButton language data SelectionMade model.selectedDemographics

                              else
                                emptyNode
                            )

                        SelectionOptionHealthCenter ->
                            let
                                options =
                                    List.map (\healthCenter -> ( healthCenter.name, healthCenter.id ))
                                        data.healthCenters
                            in
                            ( [ viewCustomSelectListInput
                                    model.selectedHealthCenter
                                    options
                                    String.fromInt
                                    SetHealthCenter
                                    "form-input select"
                                    True
                              ]
                            , emptyNode
                            )
                )
                model.populationSelection
                |> Maybe.withDefault ( [], emptyNode )

        actionButton =
            if model.selected then
                [ text <| translate language Translate.PleaseWaitMessage ]

            else
                [ actionButton_ ]
    in
    div [ class "page-content" ] <|
        viewCustomLabel language Translate.SelectViewMode ":" "header"
            :: ((populationSelectionInput :: derivedInputs) ++ actionButton)
