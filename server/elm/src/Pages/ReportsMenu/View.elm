module Pages.ReportsMenu.View exposing (view)

import App.Types exposing (Language)
import Backend.Components.Model exposing (MenuData)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton, viewHealthCenterSelection, viewPopulationSelectionInput)
import Pages.ReportsMenu.Model exposing (Model, Msg(..))
import Pages.Utils
    exposing
        ( generateReportsHeaderImage
        , viewBackendData
        , viewCustomLabel
        , viewLoadDataButton
        )
import Translate exposing (translate)


view : Language -> String -> ModelBackend -> Model -> Html Msg
view language themePath modelBackend model =
    viewBackendData modelBackend.reportsMenuData
        (\data -> viewMenu language themePath data model)


viewMenu : Language -> String -> MenuData -> Model -> Html Msg
viewMenu language themePath data model =
    let
        populationSelectionInput =
            viewPopulationSelectionInput language
                data
                [ SelectionOptionGlobal, SelectionOptionDemographics, SelectionOptionHealthCenter ]
                model.populationSelection
                SetPopulationSelection

        ( derivedInputs, actionButton_ ) =
            Maybe.map
                (\populationSelection ->
                    case populationSelection of
                        SelectionOptionDemographics ->
                            ( viewDemographicsSelection language data.site SetGeoLocation model.selectedDemographics
                            , if isJust model.selectedDemographics.province then
                                viewDemographicsSelectionActionButton language
                                    data.site
                                    "/admin/reports/statistical-queries/demographics"
                                    Translate.LoadData
                                    SelectionMade
                                    model.selectedDemographics

                              else
                                emptyNode
                            )

                        SelectionOptionGlobal ->
                            ( [], viewLoadDataButton language "/admin/reports/statistical-queries/all" SelectionMade )

                        SelectionOptionHealthCenter ->
                            viewHealthCenterSelection language
                                data
                                "/admin/reports/statistical-queries/health-center/"
                                model.selectedHealthCenter
                                SetHealthCenter
                                SelectionMade
                )
                model.populationSelection
                |> Maybe.withDefault ( [], emptyNode )

        actionButton =
            if model.selected then
                text <| translate language Translate.PleaseWaitMessage

            else
                actionButton_
    in
    div [ class "page-content reports-menu" ]
        [ generateReportsHeaderImage themePath
        , viewCustomLabel language Translate.SelectScope ":" "header"
        , div [ class "inputs" ] <|
            populationSelectionInput
                :: derivedInputs
        , div [ class "actions" ] [ actionButton ]
        ]
