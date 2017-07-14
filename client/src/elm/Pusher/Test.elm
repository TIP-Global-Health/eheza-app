module Pusher.Test exposing (all)

import Activity.Model exposing (emptyChildActivityDates)
import Expect
import Json.Decode exposing (decodeString)
import Patient.Model exposing (PatientType(..))
import Pusher.Decoder exposing (..)
import Pusher.Model exposing (..)
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
      "date_picture": null,
      "date_height" : null,
      "date_muac" : null,
      "date_progress_report" : null,
      "date_weight" : null
    }

}
            """

                    expectedResult =
                        { patientId = "100"
                        , data =
                            PatientUpdate
                                { info =
                                    PatientChild
                                        { name = "new-patient"
                                        , image = "http://placehold.it/200x200"
                                        , motherId = Just "7"
                                        , activityDates = emptyChildActivityDates
                                        }
                                }
                        }
                in
                    Expect.equal (Ok expectedResult) (decodeString decodePusherEvent json)
        ]


all : Test
all =
    describe "Pusher tests"
        [ decodeTest
        ]
