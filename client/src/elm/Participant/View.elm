module Participant.View
    exposing
        ( viewParticipantTypeFilter
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Participant.Model exposing (ParticipantTypeFilter(..))
import Translate as Trans exposing (translate, Language)


viewParticipantTypeFilter : Language -> (String -> msg) -> ParticipantTypeFilter -> Html msg
viewParticipantTypeFilter language msg participantTypeFilter =
    div []
        [ select
            [ class "ui dropdown"
            , value <| toString participantTypeFilter
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
