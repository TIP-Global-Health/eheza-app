module Pages.CompletionMenu.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict
import Backend.CompletionMenu.Model exposing (MenuData)
import Backend.Components.Model exposing (MenuScope(..))
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe.Extra exposing (isJust)
import Pages.CompletionMenu.Model exposing (..)
import Pages.Components.Model exposing (DemographicsSelection)
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.Utils exposing (populationSelectionOptionFromString, populationSelectionOptionToString)
import Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton)
import Pages.Utils
    exposing
        ( viewCustomLabel
        , viewCustomSelectListInput
        , viewGeoLocationSelectListInput
        , viewLoadDataButton
        , viewSelectListInput
        , wrapSelectListInput
        )
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


view : Language -> String -> ModelBackend -> Model -> Html Msg
view language themePath modelBackend model =
    case modelBackend.completionMenuData of
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
                    [ SelectionOptionGlobal, SelectionOptionHealthCenter ]

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
                            ( [], viewLoadDataButton language "/admin/reports/completion/all" SelectionMade )

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
                                        ("/admin/reports/completion/health-center/" ++ String.fromInt selectedHealthCenter)
                                        SelectionMade
                                )
                                model.selectedHealthCenter
                                |> Maybe.withDefault emptyNode
                            )

                        -- This option is not in use.
                        SelectionOptionDemographics ->
                            ( [], emptyNode )
                )
                model.populationSelection
                |> Maybe.withDefault ( [], emptyNode )

        actionButton =
            if model.selected then
                text <| translate language Translate.PleaseWaitMessage

            else
                actionButton_
    in
    div [ class "page-content completion-menu" ]
        [ viewCustomLabel language Translate.SelectScope ":" "header"
        , div [ class "inputs" ] <|
            populationSelectionInput
                :: derivedInputs
        , div [ class "actions" ] [ actionButton ]
        ]
