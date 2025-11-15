module Backend.ResilienceMessage.Decoder exposing (decodeResilienceMessage, decodeResilienceMessages)

import AssocList as Dict exposing (Dict)
import Backend.ResilienceMessage.Model exposing (..)
import Backend.ResilienceMessage.Utils exposing (..)
import Gizra.Json exposing (decodeInt)
import Gizra.TimePosix exposing (decodeSecondsAsPosix)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeResilienceMessages : Decoder (Dict String ResilienceMessage)
decodeResilienceMessages =
    list decodeResilienceMessage
        |> andThen
            (\messages ->
                List.map (\message -> ( generateResilienceMessageId message.category message.order, message )) messages
                    |> Dict.fromList
                    |> succeed
            )


decodeResilienceMessage : Decoder ResilienceMessage
decodeResilienceMessage =
    succeed ResilienceMessage
        |> required "resilience_category" decodeResilienceCategory
        |> required "resilience_order" decodeResilienceMessageOrder
        |> required "display_day" decodeInt
        |> optional "time_read" (nullable decodeSecondsAsPosix) Nothing
        |> optional "next_reminder" (nullable decodeSecondsAsPosix) Nothing
        |> optional "favorite_message" bool False


decodeResilienceCategory : Decoder ResilienceCategory
decodeResilienceCategory =
    string
        |> andThen
            (\s ->
                resilienceCategoryFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized ResilienceCategory."
                        )
            )


decodeResilienceMessageOrder : Decoder ResilienceMessageOrder
decodeResilienceMessageOrder =
    string
        |> andThen
            (\s ->
                resilienceMessageOrderFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized ResilienceMessageOrder."
                        )
            )
