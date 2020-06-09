module Backend.Counseling.Encoder exposing (encodeCounselingSchedule, encodeCounselingTiming, encodeCounselingTopic)

import Backend.Counseling.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeCounselingTopic : CounselingTopic -> List ( String, Value )
encodeCounselingTopic topic =
    [ ( "label", string topic.english )
    , ( "kinyarwanda_title", maybe string topic.kinyarwanda )
    , ( "type", string "counseling_topic" )
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
        , ( "topics", list encodeEntityUuid schedule.topics )
        , ( "type", string "counseling_schedule" )
        ]
