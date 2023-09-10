module Pages.Participant.Test exposing (all)

import Expect
import Test exposing (Test, describe, test)


{-| Testing auto form switch functionality for participant screen.

Can be extended meaningfully with more cases when we don't
rely on the timestamps anymore for the completed/not completed status.

-}
nextActivityTest : Test
nextActivityTest =
    test "redo nextActivityTest" <|
        always Expect.pass



{-
   let
       singleChild =
           ({ info = Participant.Model.ParticipantChild exampleChildA })
   in
       describe "A nurse visits the last activity" <|
           [ test "Then on completion, the process is over" <|
               \() ->
                   Expect.equal
                       (nextActivity ( 1, singleChild ) emptyModel)
                       Nothing
           ]
-}


all : Test
all =
    describe "Next activity flow"
        [ nextActivityTest
        ]
