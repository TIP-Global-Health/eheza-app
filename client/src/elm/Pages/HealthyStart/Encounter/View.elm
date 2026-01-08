module Pages.HealthyStart.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Translate exposing (Language)


view : Language -> NominalDate -> HealthCenterId -> HealthyStartEncounterId -> ModelIndexedDb -> Html msg
view language currentDate selectedHealthCenter id db =
    div [ class "page-encounter healthy-start" ]
        [ div [ class "ui basic head segment" ]
            [ h1 [ class "ui header" ]
                [ text "Healthy Start Encounter" ]
            ]
        , div [ class "ui full segment" ]
            [ p [] [ text "Healthy Start encounter page - to be implemented" ]
            ]
        ]
