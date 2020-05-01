module Pages.IndividualEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> Bool -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate isChw db =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate isChw db
            |> div [ class "ui full segment" ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.BeginNewEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage ClinicalPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Bool -> ModelIndexedDb -> List (Html App.Model.Msg)
viewContent language currentDate isChw db =
    let
        encounterButton encounterType =
            button
                [ class "ui primary button encounter-type"
                , onClick <| SetActivePage <| UserPage <| IndividualEncounterParticipantsPage encounterType
                ]
                [ span [ class "text" ] [ text <| translate language <| Translate.IndividualEncounterType encounterType ]
                , span [ class "icon-back" ] []
                ]

        buttons =
                [ encounterButton AntenatalEncounter
                , encounterButton NutritionEncounter
                ]
    in
    p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ] :: buttons
