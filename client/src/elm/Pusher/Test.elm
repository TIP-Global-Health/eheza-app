module Pusher.Test exposing (all)

import Activity.Model exposing (emptyChildActivityDates)
import Date
import Child.Model exposing (Gender(..))
import Expect
import Json.Decode exposing (decodeString)
import Participant.Model exposing (ParticipantType(..))
import Pusher.Decoder exposing (..)
import Pusher.Model exposing (..)
import RemoteData exposing (RemoteData(NotAsked, Success))
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
      "date_picture": null,
      "date_height" : null,
      "date_muac" : null,
      "date_progress_report" : null,
      "date_weight" : null,
      "date_birth" : "2016-08-28T10:39:49+02:00",
      "gender" : "female",
      "progress-report" : "some dummy data for now"
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
                                        , image = "https://placehold.it/200x200"
                                        , motherId = Just 7
                                        , siblingId = Just 22
                                        , examinations = NotAsked
                                        , progressReport = Success "\"some dummy data for now\""
                                        , selectedExamination = Nothing
                                        , activityDates = emptyChildActivityDates
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
      "date_picture": null,
      "date_height" : null,
      "date_muac" : null,
      "date_progress_report" : null,
      "date_weight" : null,
      "gender" : "train",
      "progress-report" : "some dummy data for now"
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
