module Backend.ResilienceMessage.Decoder exposing (decodeResilienceMessage)

import AssocList as Dict exposing (Dict)
import Backend.ResilienceMessage.Model exposing (..)
import Backend.ResilienceMessage.Utils exposing (..)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Gizra.TimePosix exposing (decodeSecondsAsPosix)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeResilienceMessage : Decoder ResilienceMessage
decodeResilienceMessage =
    succeed ResilienceMessage
        |> required "nurse" decodeEntityUuid
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
