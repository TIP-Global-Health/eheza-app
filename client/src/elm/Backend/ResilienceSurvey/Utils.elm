module Backend.ResilienceSurvey.Utils exposing (..)

import Backend.ResilienceSurvey.Model exposing (..)


resilienceSurveyTypeToString : ResilienceSurveyType -> String
resilienceSurveyTypeToString surveyType =
    case surveyType of
        ResilienceSurveyQuarterly ->
            "quarterly"

        ResilienceAdoptionSurvey ->
            "adoption"


resilienceSurveyTypeFromString : String -> Maybe ResilienceSurveyType
resilienceSurveyTypeFromString value =
    case value of
        "quarterly" ->
            Just ResilienceSurveyQuarterly

        "adoption" ->
            Just ResilienceAdoptionSurvey

        _ ->
            Nothing


resilienceSurveyQuestionToString : ResilienceSurveyQuestion -> String
resilienceSurveyQuestionToString value =
    case value of
        ResilienceSurveyQuestion1 ->
            "q1"

        ResilienceSurveyQuestion2 ->
            "q2"

        ResilienceSurveyQuestion3 ->
            "q3"

        ResilienceSurveyQuestion4 ->
            "q4"

        ResilienceSurveyQuestion5 ->
            "q5"

        ResilienceSurveyQuestion6 ->
            "q6"

        ResilienceSurveyQuestion7 ->
            "q7"

        ResilienceSurveyQuestion8 ->
            "q8"

        ResilienceSurveyQuestion9 ->
            "q9"

        ResilienceSurveyQuestion10 ->
            "q10"

        ResilienceSurveyQuestion11 ->
            "q11"

        ResilienceSurveyQuestion12 ->
            "q12"

        ResilienceSurveyQuestion13 ->
            "q13"

        ResilienceSurveyQuestion14 ->
            "q14"

        ResilienceSurveyQuestion15 ->
            "q15"

        ResilienceSurveyQuestion16 ->
            "q16"


resilienceSurveyQuestionFromString : String -> Maybe ResilienceSurveyQuestion
resilienceSurveyQuestionFromString value =
    case value of
        "q1" ->
            Just ResilienceSurveyQuestion1

        "q2" ->
            Just ResilienceSurveyQuestion2

        "q3" ->
            Just ResilienceSurveyQuestion3

        "q4" ->
            Just ResilienceSurveyQuestion4

        "q5" ->
            Just ResilienceSurveyQuestion5

        "q6" ->
            Just ResilienceSurveyQuestion6

        "q7" ->
            Just ResilienceSurveyQuestion7

        "q8" ->
            Just ResilienceSurveyQuestion8

        "q9" ->
            Just ResilienceSurveyQuestion9

        "q10" ->
            Just ResilienceSurveyQuestion10

        "q11" ->
            Just ResilienceSurveyQuestion11

        "q12" ->
            Just ResilienceSurveyQuestion12

        "q13" ->
            Just ResilienceSurveyQuestion13

        "q14" ->
            Just ResilienceSurveyQuestion14

        "q15" ->
            Just ResilienceSurveyQuestion15

        "q16" ->
            Just ResilienceSurveyQuestion16

        _ ->
            Nothing


resilienceSurveyQuestionOptionToString : ResilienceSurveyQuestionOption -> String
resilienceSurveyQuestionOptionToString value =
    case value of
        ResilienceSurveyQuestionOption0 ->
            "0"

        ResilienceSurveyQuestionOption1 ->
            "1"

        ResilienceSurveyQuestionOption2 ->
            "2"

        ResilienceSurveyQuestionOption3 ->
            "3"

        ResilienceSurveyQuestionOption4 ->
            "4"

        ResilienceSurveyQuestionOption5 ->
            "5"

        ResilienceSurveyQuestionOption6 ->
            "6"

        ResilienceSurveyQuestionOption7 ->
            "7"

        ResilienceSurveyQuestionOption8 ->
            "8"

        ResilienceSurveyQuestionOption9 ->
            "9"


resilienceSurveyQuestionOptionFromString : String -> Maybe ResilienceSurveyQuestionOption
resilienceSurveyQuestionOptionFromString value =
    case value of
        "0" ->
            Just ResilienceSurveyQuestionOption0

        "1" ->
            Just ResilienceSurveyQuestionOption1

        "2" ->
            Just ResilienceSurveyQuestionOption2

        "3" ->
            Just ResilienceSurveyQuestionOption3

        "4" ->
            Just ResilienceSurveyQuestionOption4

        "5" ->
            Just ResilienceSurveyQuestionOption5

        "6" ->
            Just ResilienceSurveyQuestionOption6

        "7" ->
            Just ResilienceSurveyQuestionOption7

        "8" ->
            Just ResilienceSurveyQuestionOption8

        "9" ->
            Just ResilienceSurveyQuestionOption9

        _ ->
            Nothing
