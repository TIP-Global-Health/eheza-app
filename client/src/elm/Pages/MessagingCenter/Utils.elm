module Pages.MessagingCenter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestion(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isNothing)
import Pages.MessagingCenter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


monthlySurveyQuestions : List ResilienceSurveyQuestion
monthlySurveyQuestions =
    [ ResilienceSurveyQuestion1
    , ResilienceSurveyQuestion2
    , ResilienceSurveyQuestion3
    , ResilienceSurveyQuestion4
    ]


resolveNumberOfUnreadMessages : NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Int
resolveNumberOfUnreadMessages currentDate nurseId nurse db =
    resolveUnreadMessages currentDate nurseId nurse db
        |> Dict.size


resolveUnreadMessages : NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveUnreadMessages currentDate nurseId nurse db =
    Maybe.map
        (\programStartDate ->
            resolveDeliveredMessages currentDate programStartDate nurseId db
                |> Dict.filter (\_ message -> isNothing message.timeRead)
        )
        nurse.resilienceProgramStartDate
        |> Maybe.withDefault (resolveDeliveredMessagesProgramNotStarted currentDate nurseId db)


resolveDeliveredMessages : NominalDate -> NominalDate -> NurseId -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveDeliveredMessages currentDate programStartDate nurseId db =
    Dict.get nurseId db.resilienceMessagesByNurse
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (Dict.filter
                (\_ message ->
                    Date.compare currentDate (Date.add Days (message.displayDay - 1) programStartDate) == GT
                )
            )
        |> Maybe.withDefault Dict.empty


resolveDeliveredMessagesProgramNotStarted : NominalDate -> NurseId -> ModelIndexedDb -> Dict ResilienceMessageId ResilienceMessage
resolveDeliveredMessagesProgramNotStarted currentDate nurseId db =
    Dict.get nurseId db.resilienceMessagesByNurse
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (Dict.filter (\_ message -> message.displayDay == 0))
        |> Maybe.withDefault Dict.empty
