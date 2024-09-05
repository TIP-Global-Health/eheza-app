module Pages.Clinical.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate healthCenterId isChw model =
    div [ class "ui basic segment page-clinical" ]
        [ viewHeader language
        , viewContent language currentDate isChw model
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


viewContent : Language -> NominalDate -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate isChw model =
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
