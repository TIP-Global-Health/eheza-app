module Backend.Session.Test exposing (all)

import Expect
import Test exposing (Test, describe, test)


{-| Tests that decodeOfflineSession can decode something in the format we
used in local storage as of 2018-11-16. This is important for back-compat,
since when we get a new version of the code, we'll need to deal with sessions
that are in progress.
-}
decodeOfflineSessionTest : Test
decodeOfflineSessionTest =
    describe "decodeOfflineSession"
        [ test "back-compat" <|
            \_ -> Expect.pass

        -- TODO: Consider deployment strategy, and whether we need back-compat
        {-
            \_ ->
           decodeString decodeOfflineSession json
               |> (\result ->
                       case result of
                           Ok _ ->
                               Expect.pass

                           Err msg ->
                               Expect.fail msg
                  )
        -}
        ]


all : Test
all =
    describe "Backend.Session"
        [ decodeOfflineSessionTest
        ]
