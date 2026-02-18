module Pages.FamilyEncounter.Activity.View exposing (view)

import Backend.Entities exposing (..)
import Backend.FamilyActivity.Model exposing (FamilyActivity(..))
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.FamilyEncounter.Activity.Model exposing (..)
import Translate exposing (Language)


view : Language -> NominalDate -> FamilyEncounterId -> FamilyActivity -> Bool -> Model -> Html Msg
view language currentDate encounterId activity isChw model =
    -- TODO: Implement activity views in #1665
    div [ class "page-activity family" ]
        [ div [ class "ui basic segment" ]
            [ h1 [] [ text <| "Family Activity - " ++ activityTitle activity ++ " - TODO: Implement in #1665" ]
            , p [] [ text "This is a placeholder for the Family Encounter activity page." ]
            ]
        ]


activityTitle : FamilyActivity -> String
activityTitle activity =
    case activity of
        FBFMother ->
            "FBF Mother"

        FBFChild ->
            "FBF Child"
