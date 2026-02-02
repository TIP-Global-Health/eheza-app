module Pages.CompletionMenu.View exposing (view)

import App.Types exposing (Language)
import Backend.CompletionMenu.Model exposing (MenuData)
import Backend.Components.Model exposing (MenuScope(..))
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.CompletionMenu.Model exposing (Model, Msg(..))
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.Utils exposing (populationSelectionOptionToString)
import Pages.Utils
    exposing
        ( viewCustomLabel
        , viewCustomSelectListInput
        , viewLoadDataButton
        , viewSelectListInput
        , wrapSelectListInput
        )
import Translate exposing (translate)


view : Language -> ModelBackend -> Model -> Html Msg
view language modelBackend model =
    case modelBackend.completionMenuData of
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
