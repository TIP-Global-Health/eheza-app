module ServiceWorker.Encoder exposing (encodeOutgoingMsg)

import Json.Encode exposing (Value, object, string)
import ServiceWorker.Model exposing (OutgoingMsg(..))


{-| Encodes a message in a way that our port can handle it.

So, obviously these could be simple strings in this case. But the general
pattern is

  - use a `tag` to give the Javascript side something easy to "switch" on
  - use additional fields to convey anything else needed

-}
encodeOutgoingMsg : OutgoingMsg -> Value
encodeOutgoingMsg msg =
    case msg of
        Register ->
            object [ ( "tag", string "Register" ) ]

        Update ->
            object [ ( "tag", string "Update" ) ]

        SkipWaiting ->
            object [ ( "tag", string "SkipWaiting" ) ]
