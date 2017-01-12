module Activity.View
    exposing
        ( viewActivityTypeFilter
        )

import Activity.Utils exposing (getActivityTypeList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Patient.Model exposing (PatientTypeFilter(..))


viewActivityTypeFilter : (String -> msg) -> PatientTypeFilter -> Html msg
viewActivityTypeFilter msg patientTypeFilter =
    div []
        (List.map
            (\activityType ->
                input
                    [ type_ "checkbox"
                    , value <| toString activityType
                    ]
                    [ text <| toString activityType ]
            )
            (getActivityTypeList patientTypeFilter)
        )
