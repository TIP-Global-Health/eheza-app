module Pages.EncounterTypes.View exposing (view)

import App.Model exposing (Msg(SetActivePage))
import Backend.Entities exposing (..)
import Backend.PrenatalParticipant.Model exposing (EncounterType(..))
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> PersonId -> Html App.Model.Msg
view language currentDate personId =
    div
        [ class "ui basic segment page-encounter-types" ]
    <|
        viewHeader language
            :: viewContent language personId


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.IndividualEncounterTypes ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage PrenatalParticipantsPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> PersonId -> List (Html App.Model.Msg)
viewContent language personId =
    [ p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
    , button
        [ class "ui primary button encounter-type"
        , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage personId
        ]
        [ span [ class "text" ] [ text <| translate language <| Translate.EncounterType AntenatalEncounter ]
        , span [ class "icon-back" ] []
        ]
    ]
