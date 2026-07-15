module Backend.Utils.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.Utils exposing (isPostInFlight)
import Expect
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Test exposing (Test, describe, test)


{-| Entities are created optimistically: the slot goes `Loading`, we POST, and
the page only moves on once the response lands. The button that started the
create stays live for that whole round-trip, so a second tap would create a
second entity - a duplicate encounter, or worse, a duplicate patient.

`isPostInFlight` is what every create handler in Backend.Update asks before it
posts. These pin the answers that matter: block while Loading, allow otherwise,
and never let one key's create block another's.

-}
all : Test
all =
    describe "isPostInFlight"
        [ test "a create already in flight for this key blocks the duplicate" <|
            \_ ->
                Dict.fromList [ ( "child", Loading ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal True
        , test "a key we have never posted for is free to create" <|
            \_ ->
                Dict.fromList [ ( "child", NotAsked ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a key absent from the dict is free to create" <|
            \_ ->
                Dict.empty
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "an earlier successful create does not block a later one" <|
            \_ ->
                -- A participant legitimately gets another encounter another day.
                Dict.fromList [ ( "child", Success () ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a failed create can be retried" <|
            \_ ->
                Dict.fromList [ ( "child", Failure Http.NetworkError ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a create in flight for one key does not block a different key" <|
            \_ ->
                let
                    dict : Dict String (WebData ())
                    dict =
                        Dict.fromList [ ( "sibling", Loading ) ]
                in
                ( isPostInFlight "sibling" dict, isPostInFlight "child" dict )
                    |> Expect.equal ( True, False )
        ]
