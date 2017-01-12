module Activity.View
    exposing
        ( viewActivityTypeFilter
        )

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityIdentity, getActivityTypeList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Patient.Model exposing (PatientTypeFilter(..))


viewActivityTypeFilter : msg -> PatientTypeFilter -> Html msg
viewActivityTypeFilter msg patientTypeFilter =
    div []
        (List.map
            (\activityType ->
                checkbox msg activityType
            )
            (getActivityTypeList patientTypeFilter)
        )


checkbox : msg -> ActivityType -> Html msg
checkbox msg activityType =
    let
        activityIdentity =
            getActivityIdentity activityType
    in
        label
            []
            [ input [ type_ "checkbox", onClick msg ] []
            , div []
                [ i [ class <| activityIdentity.icon ++ " icon" ] []
                , text activityIdentity.name
                ]
            ]
