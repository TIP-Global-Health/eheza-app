module Pages.MessagingCenter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import Backend.Model exposing (ModelIndexedDb)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestion(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.MessagingCenter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


monthlySurveyQuestions : List ResilienceSurveyQuestion
monthlySurveyQuestions =
    [ ResilienceSurveyQuestion1
    , ResilienceSurveyQuestion2
    , ResilienceSurveyQuestion3
    , ResilienceSurveyQuestion4
    ]


resolveNumberOfUnreadMessages : ModelIndexedDb -> Int
resolveNumberOfUnreadMessages db =
    8
