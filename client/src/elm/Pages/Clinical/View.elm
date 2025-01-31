module Pages.Clinical.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Utils exposing (groupEncountersEnabled, individualEncountersEnabled)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import SyncManager.Model exposing (SiteFeature)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate features healthCenterId isChw model =
    div [ class "ui basic segment page-clinical" ]
        [ viewHeader language
        , viewContent language currentDate features isChw model
            |> viewBySyncStatus language healthCenterId model.syncManager.syncInfoAuthorities
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.Clinical ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> EverySet SiteFeature -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate features isChw model =
    let
        individualEncounterButton =
            if individualEncountersEnabled isChw features then
                viewButton Translate.IndividualEncounter
                    "individual-assessment"
                    "icon-individual-encounter.svg"
                    (SetActivePage <| UserPage IndividualEncounterTypesPage)

            else
                emptyNode

        groupEncounterButton =
            if groupEncountersEnabled isChw features then
                let
                    groupAssessmentButtonAction =
                        if isChw then
                            SetActivePage <| UserPage GroupEncounterTypesPage

                        else
                            SetActivePage <| UserPage ClinicsPage
                in
                viewButton Translate.GroupAssessment
                    "group-assessment"
                    "icon-group-encounter.svg"
                    groupAssessmentButtonAction

            else
                emptyNode

        viewButton label class_ svg action =
            button
                [ class <| "ui primary button " ++ class_
                , onClick action
                ]
                [ img [ src <| "assets/images/" ++ svg ] []
                , span [ class "text" ] [ text <| translate language label ]
                , span [ class "icon-back" ] []
                ]
    in
    div []
        [ p [] [ text <| translate language Translate.WhatDoYouWantToDo ]
        , individualEncounterButton
        , groupEncounterButton
        ]
