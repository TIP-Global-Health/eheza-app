module Activity.View
    exposing
        ( viewActivityTypeFilter
        )

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityIdentity, getActivityTypeList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onWithOptions)
import Patient.Model exposing (PatientTypeFilter(..))
import Set exposing (Set)


viewActivityTypeFilter : (ActivityType -> Bool -> msg) -> PatientTypeFilter -> Set String -> Html msg
viewActivityTypeFilter msg patientTypeFilter activityTypeFilter =
    div []
        (List.map
            (\activityType ->
                checkbox msg activityType activityTypeFilter
            )
            (getActivityTypeList patientTypeFilter)
        )


checkbox : (ActivityType -> Bool -> msg) -> ActivityType -> Set String -> Html msg
checkbox msg activityType activityTypeFilter =
    let
        activityIdentity =
            getActivityIdentity activityType
    in
        -- Adding a wrapping div, so checkbox are below each other.
        div []
            [ div
                [ class "ui checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , onCheck (msg activityType)
                    , checked <| Set.member (toString activityType) activityTypeFilter
                    ]
                    []
                , label
                    []
                    [ i [ class <| activityIdentity.icon ++ " icon" ] []
                    , text activityIdentity.name
                    ]
                ]
            ]
