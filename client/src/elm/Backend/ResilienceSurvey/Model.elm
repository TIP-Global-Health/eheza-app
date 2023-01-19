module Backend.ResilienceSurvey.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias ResilienceSurvey =
    { nurse : NurseId
    , dateMeasured : NominalDate
    , surveyType : ResilienceSurveyType
    , signs : Dict ResilienceSurveyQuestion ResilienceSurveyQuestionOption
    }


type ResilienceSurveyType
    = ResilienceSurveyMonthly
    | ResilienceSurveyQuaterly


type ResilienceSurveyQuestion
    = ResilienceSurveyQuestion1
    | ResilienceSurveyQuestion2
    | ResilienceSurveyQuestion3
    | ResilienceSurveyQuestion4
    | ResilienceSurveyQuestion5
    | ResilienceSurveyQuestion6
    | ResilienceSurveyQuestion7
    | ResilienceSurveyQuestion8
    | ResilienceSurveyQuestion9
    | ResilienceSurveyQuestion10


type ResilienceSurveyQuestionOption
    = ResilienceSurveyQuestionOption0
    | ResilienceSurveyQuestionOption1
    | ResilienceSurveyQuestionOption2
    | ResilienceSurveyQuestionOption3
    | ResilienceSurveyQuestionOption4


type alias Model =
    { createResilienceSurvey : WebData () }


emptyModel : Model
emptyModel =
    { createResilienceSurvey = NotAsked }


type Msg
    = CreateResilienceSurvey ResilienceSurveyId ResilienceSurvey
    | HandleCreatedResilienceSurvey (WebData ())
