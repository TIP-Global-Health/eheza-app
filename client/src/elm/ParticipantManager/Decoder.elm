module ParticipantManager.Decoder
    exposing
        ( decodeParticipantFromResponse
        , decodeParticipantsFromResponse
        )

import Json.Decode exposing (at, Decoder)
import Participant.Model exposing (Participant, ParticipantsDict)
import Participant.Decoder exposing (decodeParticipant, decodeParticipantsDict)


decodeParticipantFromResponse : Decoder Participant
decodeParticipantFromResponse =
    at [ "data", "0" ] decodeParticipant


decodeParticipantsFromResponse : Decoder ParticipantsDict
decodeParticipantsFromResponse =
    at [ "data" ] decodeParticipantsDict
