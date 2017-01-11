module Pusher.Test exposing (all)

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
    "eventType" : "activity__update",
    "data" : {
      "type" : "child",
      "id" : "100",
      "label" : "new-patient",
      "mother": "7"
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
                                        , image = "http://placehold.it/350x150"
                                        , motherId = "7"
                                        }
                                , activities = ""
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
