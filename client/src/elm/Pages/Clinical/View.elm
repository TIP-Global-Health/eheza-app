module Pages.Clinical.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import Translate exposing (Language, translate)


view : Language -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language healthCenterId isChw model =
    div [ class "ui basic segment page-clinical" ]
        [ viewHeader language
        , viewContent language isChw
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


viewContent : Language -> Bool -> Html App.Model.Msg
viewContent language isChw =
    let
        groupAssessmentButtonAction =
            if isChw then
                SetActivePage <| UserPage GroupEncounterTypesPage

            else
                SetActivePage <| UserPage ClinicsPage

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
        , viewButton Translate.IndividualEncounter
            "individual-assessment"
            "icon-individual-encounter.svg"
            (SetActivePage <| UserPage IndividualEncounterTypesPage)
        , viewButton Translate.GroupAssessment
            "group-assessment"
            "icon-group-encounter.svg"
            groupAssessmentButtonAction
        ]
