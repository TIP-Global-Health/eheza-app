module Measurement.View
    exposing
        ( viewPatientTypeFilter
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Patient.Model exposing (PatientTypeFilter(..))
import Translate as Trans exposing (translate, Language)


viewPatientTypeFilter : Language -> (String -> msg) -> PatientTypeFilter -> Html msg
viewPatientTypeFilter language msg patientTypeFilter =
    div []
        [ select
            [ class "ui dropdown"
            , value <| toString patientTypeFilter
            , onInput msg
            ]
            (List.map
                (\filterType ->
                    option
                        [ value <| toString filterType ]
                        [ text <| toString filterType ]
                )
                [ All, Children, Mothers ]
            )
        ]
