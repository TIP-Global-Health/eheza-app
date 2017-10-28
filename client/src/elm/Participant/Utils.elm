module Participant.Utils exposing (..)

import Activity.Model exposing (ActivityType(..))
import Activity.Utils exposing (childHasPendingActivity, motherHasPendingActivity, motherHasAnyPendingActivity, childHasAnyPendingActivity)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Date exposing (Date, Day)
import Date.Extra.Facts exposing (monthFromMonthNumber)
import EveryDict exposing (EveryDict)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Participant.Model exposing (AgeDay, Participant(..), ParticipantId(..))
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


participantHasAnyPendingActivity : ParticipantId -> EditableSession -> Bool
participantHasAnyPendingActivity participantId =
    case participantId of
        ParticipantChildId childId ->
            childHasAnyPendingActivity childId

        ParticipantMotherId motherId ->
            motherHasAnyPendingActivity motherId


participantHasPendingActivity : ParticipantId -> ActivityType -> EditableSession -> Bool
participantHasPendingActivity participantId activityType =
    case participantId of
        ParticipantChildId childId ->
            case activityType of
                ChildActivity childActivity ->
                    childHasPendingActivity childId childActivity

                MotherActivity _ ->
                    always False

        ParticipantMotherId motherId ->
            case activityType of
                ChildActivity _ ->
                    always False

                MotherActivity motherActivity ->
                    motherHasPendingActivity motherId motherActivity


getParticipants : OfflineSession -> EveryDict ParticipantId Participant
getParticipants session =
    EveryDictList.foldl (\k v -> EveryDict.insert (ParticipantMotherId k) (ParticipantMother v)) EveryDict.empty session.mothers
        |> (\mothers -> EveryDict.foldl (\k v -> EveryDict.insert (ParticipantChildId k) (ParticipantChild v)) mothers session.children)
