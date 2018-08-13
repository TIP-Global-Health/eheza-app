module Backend.Counseling.Decoder exposing (..)

import Backend.Counseling.Model exposing (..)
import EveryDict
import EveryDictList
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityId)


decodeCounselingTopic : Decoder CounselingTopic
decodeCounselingTopic =
    map2 CounselingTopic
        (field "label" string)
        (nullable (field "kinyarwanda_title" string))


decodeCounselingTiming : Decoder CounselingTiming
decodeCounselingTiming =
    andThen
        (\s ->
            case s of
                "entry" ->
                    succeed Entry

                "midpoint" ->
                    succeed MidPoint

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
    decode CounselingSchedule
        |> required "timing" decodeCounselingTiming
        |> required "topics" (EveryDictList.decodeArray2 (field "id" decodeEntityId) decodeCounselingTopic)


{-| This decodes a list of `CounselingSchedule` and then combtines them into a
single `EveryCounselingSchedule` type (indexed by the `CounselingTiming`).
-}
decodeEveryCounselingSchedule : Decoder EveryCounselingSchedule
decodeEveryCounselingSchedule =
    map combineCounselingSchedules (list decodeCounselingSchedule)


{-| Combines multiple counseling schedule entities into a dictionary keyed by
the timing. Multiple entities with the same timing are combined.
-}
combineCounselingSchedules : List CounselingSchedule -> EveryCounselingSchedule
combineCounselingSchedules =
    List.foldl
        (\schedule ->
            EveryDict.update schedule.timing
                (\topics ->
                    case topics of
                        Just existing ->
                            Just <|
                                EveryDictList.union existing schedule.topics

                        Nothing ->
                            Just schedule.topics
                )
        )
        EveryDict.empty
