module Participant.Utils
    exposing
        ( getExamination
        , getParticipantAge
        , getParticipantAvatarThumb
        , getParticipantName
        , getParticipantTypeAsString
        , renderParticipantAge
        , renderParticipantDateOfBirth
        , setExamination
        )

import Date exposing (Date, Day)
import Date.Extra.Duration
import Participant.Model exposing (AgeDay, Participant(..))
import Translate as Trans exposing (translate, Language)


{-| This is a temporary convenience to get the examination for a participant.
This will need to be replaced and parameterized to deal with multiple
examinations.
-}
getExamination : Participant -> Examination
getExamination participant =
    case participant of
        ParticipantChild child ->
            List.head child.examinations
                |> Maybe.withDefault emptyExaminationChild
                |> ChildExamination

        ParticipantMother mother ->
            List.head mother.examinations
                |> Maybe.withDefault emptyExaminationMother
                |> MotherExamination


{-| Again, a temporary convenience to store an updated examination in a
participant. Note that some combinations of `Examination` and `Participant`
don't make sense, which suggests that our data model could be a bit more
sophisticated.

Eventually, this will need to be parameterized to deal with multiple
examinations.

-}
setExamination : Examination -> Participant -> Participant
setExamination examination participant =
    case ( examination, participant ) of
        ( MotherExamination motherExamination, ParticipantMother mother ) ->
            -- We just store the single examination in the list ...
            -- this is **very** temporary code!
            ParticipantMother { mother | examinations = [ motherExamination ] }

        ( ChildExamination childExamination, ParticipantChild child ) ->
            ParticipantChild { child | examinations = [ childExamination ] }

        _ ->
            -- A mismatch here, which could possibly be avoided if we thought
            -- through the types a bit more.
            participant


getParticipantAvatarThumb : Participant -> String
getParticipantAvatarThumb participant =
    case participant of
        ParticipantChild child ->
            .image child

        ParticipantMother mother ->
            .image mother


getParticipantName : Participant -> String
getParticipantName participant =
    case participant of
        ParticipantChild child ->
            .name child

        ParticipantMother mother ->
            .name mother


getParticipantTypeAsString : Participant -> String
getParticipantTypeAsString participant =
    case participant of
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
            case participant of
                ParticipantChild child ->
                    child.birthDate

                ParticipantMother mother ->
                    mother.birthDate
    in
        Participant.Model.AgeDay <|
            Date.Extra.Duration.diffDays now birthDate


renderParticipantAge : Language -> Participant -> Date -> String
renderParticipantAge language participant now =
    let
        birthDate =
            case participant of
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
            case participant of
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
