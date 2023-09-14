module Backend.ResilienceSurvey.Decoder exposing (decodeResilienceSurvey)

import AssocList as Dict
import Backend.ResilienceSurvey.Model exposing (..)
import Backend.ResilienceSurvey.Utils exposing (..)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeResilienceSurvey : Decoder ResilienceSurvey
decodeResilienceSurvey =
    succeed ResilienceSurvey
        |> required "nurse" decodeEntityUuid
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "resilience_survey_type" decodeResilienceSurveyType
        |> required "resilience_survey_signs" (list decodeResilienceSurveyQuestionTuple |> map Dict.fromList)


decodeResilienceSurveyType : Decoder ResilienceSurveyType
decodeResilienceSurveyType =
    string
        |> andThen
            (\s ->
                resilienceSurveyTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized ResilienceSurveyType."
                        )
            )


decodeResilienceSurveyQuestionTuple : Decoder ( ResilienceSurveyQuestion, ResilienceSurveyQuestionOption )
decodeResilienceSurveyQuestionTuple =
    string
        |> andThen
            (\s ->
                let
                    parts =
                        String.split "-" s

                    failure =
                        fail <|
                            s
                                ++ " is not a recognized ResilienceSurveyQuestionTuple"
                in
                case parts of
                    [ question, answer ] ->
                        Maybe.map2
                            (\decodedQuestion decodedAnswer ->
                                succeed ( decodedQuestion, decodedAnswer )
                            )
                            (resilienceSurveyQuestionFromString question)
                            (resilienceSurveyQuestionOptionFromString answer)
                            |> Maybe.withDefault failure

                    _ ->
                        failure
            )
