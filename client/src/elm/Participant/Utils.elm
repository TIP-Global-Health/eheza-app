module Participant.Utils
    exposing
        ( getParticipantAgeDays
        , getParticipantBirthDate
        , getParticipantAvatarThumb
        , getParticipantName
        , getParticipantTypeAsString
        , renderAgeMonthsDays
        , renderParticipantDateOfBirth
        )

import Date exposing (Date, Day)
import Date.Extra.Facts exposing (monthFromMonthNumber)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Participant.Model exposing (AgeDay, Participant(..))
import Translate as Trans exposing (translate, Language)
import Time.Date
import Utils.NominalDate exposing (diffDays, diffCalendarMonthsAndDays)


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


getParticipantBirthDate : Participant -> NominalDate
getParticipantBirthDate participant =
    case participant of
        ParticipantChild child ->
            child.birthDate

        ParticipantMother mother ->
            mother.birthDate


{-| Calculates the age of a participant.
To get current time, see App/Model.currentDate and
Gizra.NominalDate.fromLocalDateTime
-}
getParticipantAgeDays : Participant -> NominalDate -> AgeDay
getParticipantAgeDays participant now =
    Participant.Model.AgeDay <|
        diffDays (getParticipantBirthDate participant) now


{-| Shows the difference between the first date (the birthdate)
and the second date, formatted in months and days.
-}
renderAgeMonthsDays : Language -> NominalDate -> NominalDate -> String
renderAgeMonthsDays language birthDate now =
    let
        diff =
            diffCalendarMonthsAndDays birthDate now

        days =
            diff.days

        months =
            diff.months
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
            getParticipantBirthDate participant

        day =
            Time.Date.day birthDate

        month =
            Time.Date.month birthDate
                |> monthFromMonthNumber
                |> Trans.ResolveMonth
                |> translate language

        year =
            Time.Date.year birthDate
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
