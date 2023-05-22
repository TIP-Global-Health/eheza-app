module Gizra.TimePosix exposing (decodeSecondsAsPosix, encodePosixAsSeconds, viewTimePosix)

{-| Decodes date from string or from Epoch (i.e. number).
-}

import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, map, oneOf)
import Json.Decode.Extra exposing (datetime)
import Json.Encode exposing (Value, int)
import Time
import Translate exposing (Language, translate)


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


viewTimePosix : Language -> Time.Posix -> String
viewTimePosix language time =
    let
        normalize number =
            if number < 10 then
                "0" ++ String.fromInt number

            else
                String.fromInt number

        year =
            Time.toYear Time.utc time |> String.fromInt

        month =
            Time.toMonth Time.utc time
                |> Translate.ResolveMonth True
                |> translate language

        day =
            Time.toDay Time.utc time |> normalize

        hour =
            Time.toHour Time.utc time |> normalize

        minute =
            Time.toMinute Time.utc time |> normalize

        second =
            Time.toSecond Time.utc time |> normalize
    in
    day ++ " " ++ month ++ " " ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second ++ " UTC"
