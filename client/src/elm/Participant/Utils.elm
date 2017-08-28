module Participant.Utils
    exposing
        ( getParticipantAge
        , getParticipantAvatarThumb
        , getParticipantName
        , getParticipantTypeAsString
        , renderParticipantAge
        )

import Date exposing (Date)
import Date.Extra.Duration
import Date.Extra.Period
import Participant.Model exposing (AgeDay, Participant, ParticipantType(..))
import Translate as Trans exposing (translate, Language)


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


{-| Calculates the age of a participant.
    To get current time, see App/Model.currentDate
-}
getParticipantAge : Participant -> Date -> AgeDay
getParticipantAge participant now =
    let
        birthDate =
            case participant.info of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate

        diff =
            Date.Extra.Period.diff birthDate now
    in
        Participant.Model.AgeDay diff.day


renderParticipantAge : Language -> Participant -> Date -> String
renderParticipantAge language participant now =
    let
        birthDate =
            case participant.info of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate

        diff =
            Date.Extra.Duration.diff birthDate now

        days =
            diff.day

        months =
            diff.month
    in
        if (months == 0) then
            translate language <| Trans.AgeDays days
        else if (months == 1) then
            translate language <| Trans.AgeSingleMonth months days
        else if (days == 1) then
            translate language <| Trans.AgeSingleDay months days
        else
            translate language <| Trans.Age months days
