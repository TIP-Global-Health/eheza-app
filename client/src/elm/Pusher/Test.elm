module Pusher.Test exposing (all)

import Date
import Dict
import Expect
import Json.Decode exposing (decodeString)
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
      "id" : "100",
      "label" : "new-patient"
    }

}
            """

                    expectedResult =
                        { patientId = "100"
                        , data =
                            { name = "new-patient"
                            , image = "http://placehold.it/350x150"
                            }
                                |> PatientUpdate
                        }
                in
                    Expect.equal (Ok expectedResult) (decodeString decodePusherEvent json)
        ]


all : Test
all =
    describe "Pusher tests"
        [ decodeTest
        ]
