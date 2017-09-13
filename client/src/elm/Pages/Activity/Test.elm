module Pages.Activity.Test exposing (all)

import Date
import Dict
import Expect
import Pages.Activity.Model exposing (emptyModel)
import Pages.Activity.Update exposing (nextParticipant)
import Fixtures exposing (..)
import Pages.Participant.Utils exposing (makeLoneChildDict)
import Participant.Model
import Test exposing (Test, describe, test)


{-| Testing auto form switch functionality for activity screen.

Can be extended meaningfully with more cases when we don't
rely on the timestamps anymore for the completed/not completed status.

-}
nextParticipantTest : Test
nextParticipantTest =
    let
        -- Here, we're just finishing exampleChildA.
        noPendingDict =
            makeLoneChildDict 1 exampleChildA
    in
        describe "A nurse visits the Weight activity" <|
            [ test "Then after completing all the participants, the process is completed" <|
                \() ->
                    Expect.equal
                        (nextParticipant noPendingDict emptyModel)
                        Nothing
            ]


all : Test
all =
    describe "Next participant flow"
        [ nextParticipantTest
        ]
