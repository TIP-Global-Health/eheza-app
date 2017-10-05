module Participant.Decoder
    exposing
        ( decodeParticipant
        , decodeParticipantsDict
        )

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Mother.Decoder exposing (decodeMother)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Participant.Model exposing (..)
import Utils.Json exposing (decodeListAsIntDict)


decodeParticipant : Decoder Participant
decodeParticipant =
    decode Participant
        |> custom decodeParticipantType


decodeParticipantType : Decoder ParticipantType
decodeParticipantType =
    field "type" string
        |> andThen
            (\type_ ->
                case type_ of
                    "child" ->
                        map ParticipantChild decodeChild

                    "mother" ->
                        map ParticipantMother decodeMother

                    _ ->
                        fail (type_ ++ " is not a recognized 'type' for ParticipantType.")
            )


decodeParticipantsDict : Decoder ParticipantsDict
decodeParticipantsDict =
    decodeListAsIntDict decodeParticipant
