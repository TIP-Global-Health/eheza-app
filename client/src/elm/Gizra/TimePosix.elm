module Gizra.TimePosix exposing (decodeSecondsAsPosix, encodePosixAsSeconds)

{-| Decodes date from string or from Epoch (i.e. number).
-}

import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, map, oneOf)
import Json.Decode.Extra exposing (datetime)
import Json.Encode exposing (Value, int)
import Time


decodeDate : Decoder Time.Posix
decodeDate =
    oneOf
        [ datetime
        , decodeDateFromEpoch
        ]


{-| Decodes date from Epoch (i.e. number).
-}
decodeDateFromEpoch : Decoder Time.Posix
decodeDateFromEpoch =
    map Time.millisToPosix decodeInt


decodeSecondsAsPosix : Decoder Time.Posix
decodeSecondsAsPosix =
    map (\value -> value * 1000 |> Time.millisToPosix) decodeInt


encodePosixAsSeconds : Time.Posix -> Value
encodePosixAsSeconds value =
    int <| (Time.posixToMillis value // 1000)
