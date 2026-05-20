module Pages.Activity.Test exposing (all)

import Expect
import Test exposing (Test, describe, test)


{-| Testing auto form switch functionality for activity screen.

Can be extended meaningfully with more cases when we don't
rely on the timestamps anymore for the completed/not completed status.

-}
nextParticipantTest : Test
nextParticipantTest =
    test "redo nextParticipantTest" <|
        always Expect.pass


all : Test
all =
    describe "Next participant flow"
        [ nextParticipantTest
        ]
