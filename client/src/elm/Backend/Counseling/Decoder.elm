module Backend.Counseling.Decoder exposing (combineCounselingSchedules, decodeCounselingSchedule, decodeCounselingTiming, decodeCounselingTopic)

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (..)
import Backend.Entities exposing (..)
import Json.Decode exposing (Decoder, andThen, fail, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)
import Translate.Model exposing (TranslationSet)


decodeCounselingTopic : Decoder CounselingTopic
decodeCounselingTopic =
    succeed TranslationSet
        |> required "label" string
        |> required "kinyarwanda_title" (maybe string)
        |> hardcoded Nothing


decodeCounselingTiming : Decoder CounselingTiming
decodeCounselingTiming =
    andThen
        (\s ->
            case s of
                "entry" ->
                    succeed Entry

                "before-midpoint" ->
                    succeed BeforeMidpoint

                "midpoint" ->
                    succeed MidPoint

                "before-exit" ->
                    succeed BeforeExit

                "exit" ->
                    succeed Exit

                _ ->
                    fail <|
                        s
                            ++ " is not a recognized CounselingTiming"
        )
        string


decodeCounselingSchedule : Decoder CounselingSchedule
decodeCounselingSchedule =
    succeed CounselingSchedule
        |> required "timing" decodeCounselingTiming
        |> required "topics" (list decodeEntityUuid)


{-| Combines multiple counseling schedule entities into a dictionary keyed by
the timing. Multiple entities with the same timing are combined.
-}
combineCounselingSchedules : Dict CounselingTopicId CounselingTopic -> List CounselingSchedule -> EveryCounselingSchedule
combineCounselingSchedules allTopics =
    let
        go schedule =
            let
                -- Add the values to the ids, from our master list of all topics
                newTopics =
                    schedule.topics
                        |> List.filterMap
                            (\id ->
                                Dict.get id allTopics
                                    |> Maybe.map (\value -> ( id, value ))
                            )
                        |> Dict.fromList
            in
            Dict.update schedule.timing
                (\existingTopics ->
                    case existingTopics of
                        Just existing ->
                            -- This combines topics in the (unexpected) case
                            -- where we have more than one schedule for a
                            -- timing.
                            Just <|
                                Dict.union existing newTopics

                        Nothing ->
                            Just newTopics
                )
    in
    List.foldl go Dict.empty
