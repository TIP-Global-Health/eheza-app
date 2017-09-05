module Participant.Utils
    exposing
        ( getParticipantAge
        , getParticipantAvatarThumb
        , getParticipantName
        , getParticipantTypeAsString
        , renderParticipantAge
        , renderParticipantDateOfBirth
        )

import Date exposing (Date, Day)
import Date.Extra.Duration
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
    in
        Participant.Model.AgeDay <| Date.Extra.Duration.diffDays now birthDate


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
            Date.Extra.Duration.diff now birthDate

        days =
            diff.day

        months =
            diff.month + (diff.year * 12)
    in
        if (days == 1 && months == 0) then
            translate language <| Trans.AgeSingleDayWithoutMonth months days
        else if (months == 0) then
            translate language <| Trans.AgeDays days
        else if (months == 1 && days == 0) then
            translate language <| Trans.AgeSingleMonthWithoutDay months
        else if (months > 1 && days == 0) then
            translate language <| Trans.AgeMonthsWithoutDay months
        else if (months == 1 && days == 1) then
            translate language <| Trans.AgeSingleBoth months days
        else if (days == 1) then
            translate language <| Trans.AgeSingleDayWithMonth months days
        else if (months == 1 && days /= 0) then
            translate language <| Trans.AgeSingleMonth months days
        else
            translate language <| Trans.Age months days


renderParticipantDateOfBirth : Language -> Participant -> String
renderParticipantDateOfBirth language participant =
    let
        birthDate =
            case participant.info of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate

        day =
            Date.day birthDate

        month =
            translate language <| Trans.ResolveMonth <| Date.month birthDate

        year =
            Date.year birthDate
    in
        (if day < 10 then
            "0" ++ toString day
         else
            toString day
        )
            ++ " "
            ++ month
            ++ " "
            ++ toString year
