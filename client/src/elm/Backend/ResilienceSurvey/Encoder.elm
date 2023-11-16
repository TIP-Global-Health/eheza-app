module Backend.ResilienceSurvey.Encoder exposing (encodeResilienceSurvey)

import AssocList as Dict
import Backend.ResilienceSurvey.Model exposing (..)
import Backend.ResilienceSurvey.Utils exposing (..)
import Gizra.NominalDate
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeResilienceSurvey : ResilienceSurvey -> List ( String, Value )
encodeResilienceSurvey survey =
    let
        signs =
            Dict.toList survey.signs
                |> List.map
                    (\( question, answer ) ->
                        resilienceSurveyQuestionToString question
                            ++ "-"
                            ++ resilienceSurveyQuestionOptionToString answer
                    )
    in
    [ ( "nurse", encodeEntityUuid survey.nurse )
    , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD survey.dateMeasured )
    , ( "resilience_survey_type", encodeResilienceSurveyType survey.surveyType )
    , ( "resilience_survey_signs", list string signs )
    , ( "deleted", bool False )
    , ( "type", string "resilience_survey" )
    ]


encodeResilienceSurveyType : ResilienceSurveyType -> Value
encodeResilienceSurveyType =
    resilienceSurveyTypeToString >> string
