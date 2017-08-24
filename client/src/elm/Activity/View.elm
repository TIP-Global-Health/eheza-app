module Activity.View
    exposing
        ( viewActivityTypeFilter
        )

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityIdentity, getActivityTypeList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onWithOptions)
import Participant.Model exposing (ParticipantTypeFilter(..))


viewActivityTypeFilter : (ActivityType -> Bool -> msg) -> ParticipantTypeFilter -> List ActivityType -> Html msg
viewActivityTypeFilter msg participantTypeFilter activityTypeFilter =
    div []
        (List.map
            (\activityType ->
                checkbox msg activityType activityTypeFilter
            )
            (getActivityTypeList participantTypeFilter)
        )


checkbox : (ActivityType -> Bool -> msg) -> ActivityType -> List ActivityType -> Html msg
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
                    , checked <| List.member activityType activityTypeFilter
                    ]
                    []
                , label
                    []
                    [ i [ class <| activityIdentity.icon ++ " icon" ] []
                    , text activityIdentity.name
                    ]
                ]
            ]
