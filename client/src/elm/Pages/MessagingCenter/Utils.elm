module Pages.MessagingCenter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceMessage.Utils exposing (emptyMessagesDict, generateEmptyMessagesByProgramStartDate)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestion(..), ResilienceSurveyType(..))
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.MessagingCenter.Model exposing (SurveyForm, SurveyScoreDialogState(..))
import RemoteData
import Time exposing (posixToMillis)


quarterlySurveyQuestions : List ResilienceSurveyQuestion
quarterlySurveyQuestions =
    [ ResilienceSurveyQuestion1
    , ResilienceSurveyQuestion2
    , ResilienceSurveyQuestion3
    , ResilienceSurveyQuestion4
    ]


adoptionSurveyQuestions : List ResilienceSurveyQuestion
adoptionSurveyQuestions =
    [ ResilienceSurveyQuestion1
    , ResilienceSurveyQuestion2
    , ResilienceSurveyQuestion3
    , ResilienceSurveyQuestion4
    , ResilienceSurveyQuestion5
    , ResilienceSurveyQuestion6
    , ResilienceSurveyQuestion7
    , ResilienceSurveyQuestion8
    , ResilienceSurveyQuestion9
    , ResilienceSurveyQuestion10
    , ResilienceSurveyQuestion11
    , ResilienceSurveyQuestion12
    ]


generateInboxMessages : NominalDate -> NominalDate -> Dict String ResilienceMessage -> Dict String ResilienceMessage
generateInboxMessages currentDate programStartDate recordedMessage =
    generateEmptyMessagesByProgramStartDate currentDate programStartDate
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


resolveNumberOfUnreadMessages : Time.Posix -> NominalDate -> Nurse -> Int
resolveNumberOfUnreadMessages currentTime currentDate nurse =
    Maybe.map
        (\programStartDate ->
            generateInboxMessages currentDate programStartDate nurse.resilienceMessages
                |> Dict.filter (\_ message -> isMessageUnread currentTime message)
        )
        nurse.resilienceProgramStartDate
        |> Maybe.withDefault
            (Dict.filter (\_ message -> message.displayDay == 0) emptyMessagesDict)
        |> Dict.size


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


surveyQuestionsAnswered : ResilienceSurveyType -> SurveyForm -> Bool
surveyQuestionsAnswered surveyType surveyForm =
    let
        surveyQuestions =
            case surveyType of
                ResilienceSurveyQuarterly ->
                    quarterlySurveyQuestions

                ResilienceSurveyAdoption ->
                    adoptionSurveyQuestions
    in
    Dict.size surveyForm == List.length surveyQuestions


resolveSurveyScoreDialogState : ResilienceSurveyType -> Int -> SurveyScoreDialogState
resolveSurveyScoreDialogState surveyType score =
    case surveyType of
        ResilienceSurveyQuarterly ->
            QuarterlySurveyScore score

        ResilienceSurveyAdoption ->
            AdoptionSurveyScore score
