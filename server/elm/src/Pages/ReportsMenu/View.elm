module Pages.ReportsMenu.View exposing (view)

import App.Types exposing (Language)
import Backend.Components.Model exposing (MenuScope(..))
import Backend.Model exposing (ModelBackend)
import Backend.ReportsMenu.Model exposing (MenuData)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.Utils exposing (populationSelectionOptionToString)
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton)
import Pages.ReportsMenu.Model exposing (Model, Msg(..))
import Pages.Utils
    exposing
        ( generateReportsHeaderImage
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewLoadDataButton
        , viewSelectListInput
        , wrapSelectListInput
        )
import Translate exposing (translate)


view : Language -> String -> ModelBackend -> Model -> Html Msg
view language themePath modelBackend model =
    case modelBackend.reportsMenuData of
        Just (Ok data) ->
            viewMenu language themePath data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewMenu : Language -> String -> MenuData -> Model -> Html Msg
viewMenu language themePath data model =
    let
        populationSelectionInput =
            let
                allOptions =
                    [ SelectionOptionGlobal, SelectionOptionDemographics, SelectionOptionHealthCenter ]

                options =
                    Maybe.map
                        (\scope ->
                            case scope of
                                ScopeFull ->
                                    allOptions

                                ScopeHealthCenters ->
                                    [ SelectionOptionHealthCenter ]
                        )
                        data.scope
                        |> Maybe.withDefault allOptions
            in
            viewSelectListInput language
                model.populationSelection
                options
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
                            ( [], viewLoadDataButton language "/admin/reports/statistical-queries/all" SelectionMade )

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
                                    (Just "")
                                    |> wrapSelectListInput language Translate.HealthCenter False
                              ]
                            , Maybe.map
                                (\selectedHealthCenter ->
                                    viewLoadDataButton language
                                        ("/admin/reports/statistical-queries/health-center/" ++ String.fromInt selectedHealthCenter)
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
    div [ class "page-content reports-menu" ]
        [ generateReportsHeaderImage themePath
        , viewCustomLabel language Translate.SelectScope ":" "header"
        , div [ class "inputs" ] <|
            populationSelectionInput
                :: derivedInputs
        , div [ class "actions" ] [ actionButton ]
        ]
