module Gizra.Date exposing (decodeDate)

{-| Decodes date from string or from Epoch (i.e. number).
-}

import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, map, oneOf)
import Json.Decode.Extra exposing (datetime)
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
