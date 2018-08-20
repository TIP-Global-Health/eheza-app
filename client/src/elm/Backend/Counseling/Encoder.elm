module Backend.Counseling.Encoder exposing (..)

import Backend.Counseling.Model exposing (..)
import EveryDict
import EveryDictList
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityId)


encodeCounselingTopic : CounselingTopic -> List ( String, Value )
encodeCounselingTopic topic =
    [ ( "label", string topic.english )
    , ( "kinyarwanda_title", maybe string topic.kinyarwanda )
    ]


encodeCounselingTiming : CounselingTiming -> Value
encodeCounselingTiming timing =
    case timing of
        Entry ->
            string "entry"

        BeforeMidpoint ->
            string "before-midpoint"

        MidPoint ->
            string "midpoint"

        BeforeExit ->
            string "before-exit"

        Exit ->
            string "exit"


encodeCounselingSchedule : CounselingSchedule -> Value
encodeCounselingSchedule schedule =
    object
        [ ( "timing", encodeCounselingTiming schedule.timing )
        , ( "topics"
          , EveryDictList.toList schedule.topics
                |> List.map (\( id, topic ) -> object (( "id", encodeEntityId id ) :: encodeCounselingTopic topic))
                |> list
          )
        ]


{-| This encodes a structure representing the whole counseling schedule
as a list of "Counseling Schedule" entities.
-}
encodeEveryCounselingSchedule : EveryCounselingSchedule -> Value
encodeEveryCounselingSchedule =
    EveryDict.toList
        >> List.map
            (\( timing, topics ) ->
                encodeCounselingSchedule
                    { timing = timing
                    , topics = topics
                    }
            )
        >> list
