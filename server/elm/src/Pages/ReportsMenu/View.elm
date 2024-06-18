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
import List.Extra
import Maybe.Extra exposing (isJust)
import Pages.Components.Model exposing (DemographicsSelection)
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton)
import Pages.ReportsMenu.Model exposing (..)
import Pages.ReportsMenu.Types exposing (..)
import Pages.ReportsMenu.Utils exposing (populationSelectionOptionToString)
import Pages.Utils
    exposing
        ( viewCustomLabel
        , viewCustomSelectListInput
        , viewGenerateReportButton
        , viewGeoLocationSelectListInput
        , viewSelectListInput
        , wrapSelectListInput
        )
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
            viewSelectListInput language
                model.populationSelection
                [ SelectionOptionGlobal, SelectionOptionDemographics, SelectionOptionHealthCenter ]
                populationSelectionOptionToString
                SetPopulationSelection
                Translate.PopulationSelectionOption
                "select-input"
                |> wrapSelectListInput language Translate.Scope False

        ( derivedInputs, actionButton_ ) =
            Maybe.map
                (\populationSelection ->
                    case populationSelection of
                        SelectionOptionGlobal ->
                            ( [], viewGenerateReportButton language "/admin/reports/aggregated-reports/all" SelectionMade )

                        SelectionOptionDemographics ->
                            ( viewDemographicsSelection language data.site SetGeoLocation model.selectedDemographics
                            , if isJust model.selectedDemographics.province then
                                viewDemographicsSelectionActionButton language
                                    data.site
                                    "/admin/reports/aggregated-reports/demographics"
                                    SelectionMade
                                    model.selectedDemographics

                              else
                                emptyNode
                            )

                        SelectionOptionHealthCenter ->
                            let
                                options =
                                    List.sortBy .name data.healthCenters
                                        |> List.map (\healthCenter -> ( healthCenter.name, healthCenter.id ))
                            in
                            ( [ viewCustomSelectListInput
                                    model.selectedHealthCenter
                                    options
                                    String.fromInt
                                    SetHealthCenter
                                    "select-input"
                                    True
                                    |> wrapSelectListInput language Translate.HealthCenter False
                              ]
                            , Maybe.map
                                (\selectedHealthCenter ->
                                    viewGenerateReportButton language
                                        ("/admin/reports/aggregated-reports/health-center/" ++ String.fromInt selectedHealthCenter)
                                        SelectionMade
                                )
                                model.selectedHealthCenter
                                |> Maybe.withDefault emptyNode
                            )
                )
                model.populationSelection
                |> Maybe.withDefault ( [], emptyNode )

        actionButton =
            if model.selected then
                text <| translate language Translate.PleaseWaitMessage

            else
                actionButton_
    in
    div [ class "page-content" ]
        [ viewCustomLabel language Translate.SelectScope ":" "header"
        , div [ class "inputs" ] <|
            populationSelectionInput
                :: derivedInputs
        , div [ class "actions" ] [ actionButton ]
        ]
