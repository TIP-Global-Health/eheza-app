module Pages.FamilyEncounter.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.FamilyEncounter.Encounter.Model exposing (..)
import Translate exposing (Language)


view : Language -> NominalDate -> FamilyEncounterId -> Bool -> Model -> Html Msg
view language currentDate encounterId isChw model =
    -- TODO: Implement view in #1665
    div [ class "page-encounter family" ]
        [ div [ class "ui basic segment" ]
            [ h1 [] [ text "Family Encounter - TODO: Implement in #1665" ]
            , p [] [ text "This is a placeholder for the Family Encounter page." ]
            ]
        ]
