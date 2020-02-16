module Pages.Clinical.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language, translate)


view : Language -> Html App.Model.Msg
view language =
    div
        [ class "ui basic segment page-clinical" ]
    <|
        viewHeader language
            :: viewContent language


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.Clinical ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> List (Html App.Model.Msg)
viewContent language =
    let
        groupAssessmentButton =
            button
                [ class "ui primary button group-assessment"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage Nothing
                ]
                [ span [ class "icon" ] []
                , span [ class "text" ] [ text <| translate language Translate.GroupAssessment ]
                , span [ class "icon-back" ] []
                ]

        individualEncounterButton =
            button
                [ class "ui primary button individual-assessment"
                , onClick <| SetActivePage <| UserPage IndividualEncounterTypesPage
                ]
                [ span [ class "icon" ] []
                , span [ class "text" ] [ text <| translate language Translate.IndividualEncounter ]
                , span [ class "icon-back" ] []
                ]
    in
    [ p [] [ text <| translate language Translate.WhatDoYouWantToDo ]
    , individualEncounterButton
    , groupAssessmentButton
    ]
