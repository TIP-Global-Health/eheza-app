module Pages.MessagingCenter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceMessage.Utils exposing (generateEmptyMessagesByProgramStartDate)
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


generateInboxMessages : NurseId -> NominalDate -> NominalDate -> Dict String ResilienceMessage -> Dict String ResilienceMessage
generateInboxMessages nurseId currentDate programStartDate recordedMessage =
    generateEmptyMessagesByProgramStartDate nurseId currentDate programStartDate
        |> Dict.foldl
            (\key value accum ->
                Dict.get key recordedMessage
                    |> Maybe.map
                        (\recordedValue ->
                            Dict.insert key recordedValue accum
                        )
                    |> Maybe.withDefault (Dict.insert key value accum)
            )
            Dict.empty


resolveInboxMessagesProgramNotStarted : NominalDate -> NurseId -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveInboxMessagesProgramNotStarted currentDate nurseId db =
    Dict.get nurseId db.resilienceMessagesByNurse
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (Dict.filter (\_ message -> message.displayDay == 0))
        |> Maybe.withDefault Dict.empty
