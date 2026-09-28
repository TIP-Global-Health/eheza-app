module Pages.CompletionMenu.View exposing (view)

import App.Types exposing (Language)
import Backend.Components.Model exposing (MenuData)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.CompletionMenu.Model exposing (Model, Msg(..))
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.View exposing (viewHealthCenterSelection, viewPopulationSelectionInput)
import Pages.Utils exposing (viewBackendData, viewCustomLabel, viewLoadDataButton)
import Translate exposing (translate)


view : Language -> ModelBackend -> Model -> Html Msg
view language modelBackend model =
    viewBackendData modelBackend.completionMenuData
        (\data -> viewMenu language data model)


viewMenu : Language -> MenuData -> Model -> Html Msg
viewMenu language data model =
    let
        populationSelectionInput =
            viewPopulationSelectionInput language
                data
                [ SelectionOptionGlobal, SelectionOptionHealthCenter ]
                model.populationSelection
                SetPopulationSelection

        ( derivedInputs, actionButton_ ) =
            Maybe.map
                (\populationSelection ->
                    case populationSelection of
                        -- This option is not in use.
                        SelectionOptionDemographics ->
                            ( [], emptyNode )

                        SelectionOptionGlobal ->
                            ( [], viewLoadDataButton language "/admin/reports/completion/all" SelectionMade )

                        SelectionOptionHealthCenter ->
                            viewHealthCenterSelection language
                                data
                                "/admin/reports/completion/health-center/"
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
    div [ class "page-content completion-menu" ]
        [ viewCustomLabel language Translate.SelectScope ":" "header"
        , div [ class "inputs" ] <|
            populationSelectionInput
                :: derivedInputs
        , div [ class "actions" ] [ actionButton ]
        ]
