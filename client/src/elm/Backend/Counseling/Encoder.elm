module Backend.Counseling.Encoder exposing (..)

import Backend.Counseling.Model exposing (..)
import Json.Encode exposing (..)


encodeCounselingTopic : CounselingTopic -> Value
encodeCounselingTopic topic =
    object
        [ ( "label", string topic.english )
        , ( "kinyarwanda_title", string topic.kinyarwanda )
        ]


encodeCounselingTiming : CounselingTiming -> Value
encodeCounselingTiming timing =
    case timing of
        Entry ->
            string "entry"

        MidPoint ->
            string "midpoint"

        Exit ->
            string "exit"
