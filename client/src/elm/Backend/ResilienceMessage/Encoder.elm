module Backend.ResilienceMessage.Encoder exposing (encodeResilienceMessage)

import AssocList as Dict exposing (Dict)
import Backend.ResilienceMessage.Model exposing (..)
import Backend.ResilienceMessage.Utils exposing (..)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Gizra.TimePosix exposing (encodePosixAsSeconds)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeResilienceMessage : ResilienceMessage -> List ( String, Value )
encodeResilienceMessage message =
    let
        read =
            Maybe.map (\timeRead -> [ ( "time_read", encodePosixAsSeconds timeRead ) ]) message.timeRead
                |> Maybe.withDefault []

        reminder =
            Maybe.map (\nextReminder -> [ ( "next_reminder", encodePosixAsSeconds nextReminder ) ]) message.nextReminder
                |> Maybe.withDefault []
    in
    [ ( "nurse", encodeEntityUuid message.nurse )
    , ( "resilience_category", encodeResilienceCategory message.category )
    , ( "resilience_order", encodeResilienceMessageOrder message.order )
    , ( "display_day", int message.displayDay )
    , ( "favorite_message", bool message.isFavorite )
    , ( "deleted", bool False )
    , ( "type", string "resilience_message" )
    ]
        ++ read
        ++ reminder


encodeResilienceCategory : ResilienceCategory -> Value
encodeResilienceCategory =
    resilienceCategoryToString >> string


encodeResilienceMessageOrder : ResilienceMessageOrder -> Value
encodeResilienceMessageOrder =
    resilienceMessageOrderToString >> string
