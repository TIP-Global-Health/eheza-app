module Participant.Utils
    exposing
        ( getParticipantAvatarThumb
        , getParticipantName
        , getParticipantTypeAsString
        )

import Participant.Model exposing (Participant, ParticipantType(..))


getParticipantAvatarThumb : Participant -> String
getParticipantAvatarThumb participant =
    case participant.info of
        ParticipantChild child ->
            .image child

        ParticipantMother mother ->
            .image mother


getParticipantName : Participant -> String
getParticipantName participant =
    case participant.info of
        ParticipantChild child ->
            .name child

        ParticipantMother mother ->
            .name mother


getParticipantTypeAsString : Participant -> String
getParticipantTypeAsString participant =
    case participant.info of
        ParticipantChild child ->
            "child"

        ParticipantMother mother ->
            "mother"
