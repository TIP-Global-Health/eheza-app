module Pages.Activity.Test exposing (all)

import Date
import Dict
import Expect
import Pages.Activity.Model exposing (emptyModel)
import Fixtures exposing (..)
import Participant.Model
import Restful.Endpoint exposing (toEntityId)
import Test exposing (Test, describe, test)


{-| Testing auto form switch functionality for activity screen.

Can be extended meaningfully with more cases when we don't
rely on the timestamps anymore for the completed/not completed status.

-}
nextParticipantTest : Test
nextParticipantTest =
    test "redo nextParticipantTest" <|
        always Expect.pass



{-
   let
       -- Here, we're just finishing exampleChildA.
       noPendingDict =
           makeLoneChildDict (toEntityId 1) exampleChildA
   in
       describe "A nurse visits the Weight activity" <|
           [ test "Then after completing all the participants, the process is completed" <|
               \() ->
                   Expect.equal
                       (nextParticipant noPendingDict emptyModel)
                       Nothing
           ]
-}


all : Test
all =
    describe "Next participant flow"
        [ nextParticipantTest
        ]
