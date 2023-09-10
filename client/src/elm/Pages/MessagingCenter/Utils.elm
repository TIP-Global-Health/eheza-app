module Pages.MessagingCenter.Utils exposing (monthlySurveyQuestions, resolveInboxMessages, resolveNumberOfUnreadMessages)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestion(..))
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import RemoteData
import Time exposing (posixToMillis)


monthlySurveyQuestions : List ResilienceSurveyQuestion
monthlySurveyQuestions =
    [ ResilienceSurveyQuestion1
    , ResilienceSurveyQuestion2
    , ResilienceSurveyQuestion3
    , ResilienceSurveyQuestion4
    ]


resolveNumberOfUnreadMessages : Time.Posix -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Int
resolveNumberOfUnreadMessages currentTime currentDate nurseId nurse db =
    resolveUnreadMessages currentTime currentDate nurseId nurse db
        |> Dict.size


resolveUnreadMessages : Time.Posix -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveUnreadMessages currentTime currentDate nurseId nurse db =
    Maybe.map
        (\programStartDate ->
            resolveInboxMessages currentDate programStartDate nurseId db
                |> Dict.filter (\_ message -> isMessageUnread currentTime message)
        )
        nurse.resilienceProgramStartDate
        |> Maybe.withDefault (resolveInboxMessagesProgramNotStarted currentDate nurseId db)


isMessageUnread : Time.Posix -> ResilienceMessage -> Bool
isMessageUnread currentTime message =
    case message.timeRead of
        Nothing ->
            -- Read time not set => message is unread.
            True

        Just timeRead ->
            Maybe.map
                (\nextReminder ->
                    let
                        nextReminderMillis =
                            posixToMillis nextReminder
                    in
                    -- Reminder was set to latter time than the
                    -- time at which message was read.
                    (nextReminderMillis > posixToMillis timeRead)
                        && -- Scheduled reminder time was reached.
                           (posixToMillis currentTime >= nextReminderMillis)
                )
                message.nextReminder
                |> -- No reminder set but read time was set =>
                   -- messages was read.
                   Maybe.withDefault False


resolveInboxMessages : NominalDate -> NominalDate -> NurseId -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveInboxMessages currentDate programStartDate nurseId db =
    Dict.get nurseId db.resilienceMessagesByNurse
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (Dict.filter
                (\_ message ->
                    Date.compare currentDate (Date.add Days (message.displayDay - 1) programStartDate) == GT
                )
            )
        |> Maybe.withDefault Dict.empty


resolveInboxMessagesProgramNotStarted : NominalDate -> NurseId -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveInboxMessagesProgramNotStarted currentDate nurseId db =
    Dict.get nurseId db.resilienceMessagesByNurse
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (Dict.filter (\_ message -> message.displayDay == 0))
        |> Maybe.withDefault Dict.empty
