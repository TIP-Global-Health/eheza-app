module Backend.ResilienceMessage.Encoder exposing (encodeResilienceMessage)

import Backend.ResilienceMessage.Model exposing (..)
import Backend.ResilienceMessage.Utils exposing (..)
import Gizra.TimePosix exposing (encodePosixAsSeconds)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeNullable)


encodeResilienceMessage : ResilienceMessage -> List ( String, Value )
encodeResilienceMessage message =
    [ ( "resilience_category", encodeResilienceCategory message.category )
    , ( "resilience_order", encodeResilienceMessageOrder message.order )
    , ( "display_day", int message.displayDay )
    , ( "favorite_message", bool message.isFavorite )
    ]
        ++ encodeNullable "time_read" message.timeRead encodePosixAsSeconds
        ++ encodeNullable "next_reminder" message.nextReminder encodePosixAsSeconds


encodeResilienceCategory : ResilienceCategory -> Value
encodeResilienceCategory =
    resilienceCategoryToString >> string


encodeResilienceMessageOrder : ResilienceMessageOrder -> Value
encodeResilienceMessageOrder =
    resilienceMessageOrderToString >> string
