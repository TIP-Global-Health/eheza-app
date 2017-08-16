module Pusher.Decoder exposing (decodePusherEvent)

import Json.Decode as Json exposing (Decoder, andThen, at, fail, field, int, map, map2, string)
import Json.Decode.Pipeline exposing (custom, decode, optional, required, requiredAt)
import Pusher.Model exposing (..)
import Participant.Decoder exposing (decodeParticipant)
import Participant.Model exposing (Participant)
import Utils.Json exposing (decodeInt)


decodePusherEvent : Decoder PusherEvent
decodePusherEvent =
    decode PusherEvent
        |> requiredAt [ "data", "id" ] decodeInt
        |> custom decodePusherEventData


decodePusherEventData : Decoder PusherEventData
decodePusherEventData =
    field "eventType" string
        |> andThen
            (\type_ ->
                case type_ of
                    "patient__update" ->
                        map ParticipantUpdate decodeParticipantUpdateData

                    _ ->
                        fail (type_ ++ " is not a recognized 'type' for PusherEventData.")
            )


decodeParticipantUpdateData : Decoder Participant
decodeParticipantUpdateData =
    field "data" decodeParticipant
