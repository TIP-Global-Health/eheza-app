module Pusher.Test exposing (all)

import Date
import Backend.Child.Model exposing (Gender(..))
import Drupal.Restful exposing (toEntityId)
import Expect
import Json.Decode exposing (decodeString)
import Participant.Model exposing (ParticipantType(..))
import Pusher.Decoder exposing (..)
import Pusher.Model exposing (..)
import RemoteData exposing (RemoteData(NotAsked))
import Test exposing (Test, describe, test)


decodeTest : Test
decodeTest =
    describe "Decode Pusher"
        [ test "valid json" <|
            \() ->
                let
                    json =
                        """
{
    "eventType" : "patient__update",
    "data" : {
      "type" : "child",
      "id" : "100",
      "label" : "new-patient",
      "mother": "7",
      "sibling": "22",
      "examinations": [],
      "date_birth" : "2016-08-28T10:39:49+02:00",
      "gender" : "female"
    }

}
            """

                    expectedResult =
                        { participantId = 100
                        , data =
                            ParticipantUpdate
                                { info =
                                    ParticipantChild
                                        { name = "new-patient"
                                        , image = ""
                                        , motherId = Just (toEntityId 7)
                                        , siblingId = Just (toEntityId 22)
                                        , examinations = []
                                        , birthDate = Date.fromTime 1472373589000
                                        , gender = Female
                                        }
                                }
                        }
                in
                    Expect.equal (Ok expectedResult) (decodeString decodePusherEvent json)
        , test "invalid gender" <|
            \() ->
                let
                    json =
                        """
{
    "eventType" : "patient__update",
    "data" : {
      "type" : "child",
      "id" : "100",
      "label" : "new-patient",
      "mother": "7",
      "examinations": [],
      "gender" : "train"
    }

}
            """
                in
                    Expect.equal (Err "I ran into a `fail` decoder at _.data.gender: train is not a recognized 'type' for Gender.") (decodeString decodePusherEvent json)
        ]


all : Test
all =
    describe "Pusher tests"
        [ decodeTest
        ]
