module Pages.PregnancyOutcome.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Backend.PrenatalParticipant.Model exposing (EncounterType(..))
import EveryDict
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PregnancyOutcome.Model exposing (Model, Msg(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> PrenatalParticipantId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    div
        [ class "wrap wrap-alt-2 page-pregnancy-outcome" ]
        [ viewHeader language id
        , viewContent language currentDate id db
            |> div [ class "ui full segment" ]
        ]


viewHeader : Language -> PrenatalParticipantId -> Html Msg
viewHeader language id =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PregnancyOutcomeLabel ]
        , a
            [ class "link-back"

            -- Todo: resolve person ID from participant ID.
            -- , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> PrenatalParticipantId -> ModelIndexedDb -> List (Html Msg)
viewContent language currentDate id db =
    [ div [] [ text "hello" ] ]
