module Backend.ResilienceMessage.Decoder exposing (decodeReasonForNotConsenting, decodeResilienceMessages)

import AssocList as Dict exposing (Dict)
import Backend.ResilienceMessage.Model exposing (ReasonForNotConsenting(..), ResilienceCategory, ResilienceMessage, ResilienceMessageOrder)
import Backend.ResilienceMessage.Utils exposing (generateResilienceMessageId, resilienceCategoryFromString, resilienceMessageOrderFromString)
import Gizra.Json exposing (decodeInt)
import Gizra.TimePosix exposing (decodeSecondsAsPosix)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeResilienceMessages : Decoder (Dict String ResilienceMessage)
decodeResilienceMessages =
    list decodeResilienceMessage
        |> map
            (\messages ->
                List.map (\message -> ( generateResilienceMessageId message.category message.order, message )) messages
                    |> Dict.fromList
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


decodeReasonForNotConsenting : Decoder ReasonForNotConsenting
decodeReasonForNotConsenting =
    string
        |> andThen
            (\str ->
                case str of
                    "many-other-commitments" ->
                        succeed ManyOtherCommitments

                    "no-dedicated-time" ->
                        succeed NoDedicatedTimeForTheProgram

                    "program-not-addressing-stressors" ->
                        succeed ProgramNotAddressingMyStressors

                    "do-not-want-to-be-seen-as-struggling" ->
                        succeed DontWantToBeSeenAsStruggling

                    "tried-similar-program" ->
                        succeed TriedSimilarProgramBefore

                    "not-interested" ->
                        succeed NotInterestedInProgram

                    _ ->
                        fail ("Unknown reason for not consenting: " ++ str)
            )
