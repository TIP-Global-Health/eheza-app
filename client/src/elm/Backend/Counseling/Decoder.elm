module Backend.Counseling.Decoder exposing (..)

import Backend.Counseling.Model exposing (..)
import Json.Decode exposing (..)


decodeCounselingTopic : Decoder CounselingTopic
decodeCounselingTopic =
    map2 CounselingTopic
        (field "label" string)
        (field "kinyarwanda_title" string)


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
