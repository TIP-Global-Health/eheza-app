module Activity.View exposing (viewActivityTypeFilter)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityIcon, getActivityTypeList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onWithOptions)
import Participant.Model exposing (ParticipantTypeFilter(..))
import Translate exposing (Language, translate)


viewActivityTypeFilter : Language -> (ActivityType -> Bool -> msg) -> ParticipantTypeFilter -> List ActivityType -> Html msg
viewActivityTypeFilter language msg participantTypeFilter activityTypeFilter =
    div []
        (List.map
            (\activityType ->
                checkbox language msg activityType activityTypeFilter
            )
            (getActivityTypeList participantTypeFilter)
        )


checkbox : Language -> (ActivityType -> Bool -> msg) -> ActivityType -> List ActivityType -> Html msg
checkbox language msg activityType activityTypeFilter =
    -- Adding a wrapping div, so checkbox are below each other.
    div []
        [ div
            [ class "ui checkbox activity" ]
            [ input
                [ type_ "checkbox"
                , onCheck (msg activityType)
                , checked <| List.member activityType activityTypeFilter
                ]
                []
            , label
                []
                [ i [ getActivityIcon activityType ] []
                , text <| translate language (Translate.ActivitiesTitle activityType)
                ]
            ]
        ]
