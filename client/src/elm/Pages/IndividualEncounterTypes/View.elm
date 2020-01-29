module Pages.IndividualEncounterTypes.View exposing (view)

import App.Model exposing (Msg(SetActivePage))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import EveryDict
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate db =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate db
            |> div [ class "ui full segment" ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.BegingNewEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage ClinicalPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> List (Html App.Model.Msg)
viewContent language currentDate db =
    let
        antenatalButton =
            button
                [ class "ui primary button encounter-type"
                , onClick <| SetActivePage <| UserPage <| IndividualEncounterParticipantsPage AntenatalEncounter
                ]
                [ span [ class "text" ] [ text <| translate language <| Translate.IndividualEncounterType AntenatalEncounter ]
                , span [ class "icon-back" ] []
                ]
    in
    [ p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
    , antenatalButton
    ]
