module Participant.View
    exposing
        ( getParticipantAge
        , renderParticipantAge
        , viewParticipantTypeFilter
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Participant.Model exposing (AgeDay, Participant, ParticipantTypeFilter(..))
import Translate as Trans exposing (translate, Language)


getParticipantAge : Participant -> AgeDay
getParticipantAge participant =
    Participant.Model.AgeDay 10


renderParticipantAge : Participant -> String
renderParticipantAge participant =
    let
        ageDay =
            getParticipantAge participant
    in
        "4 months and 2 days"


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
