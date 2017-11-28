module ServiceWorker.Encoder exposing (..)

import Json.Encode exposing (..)
import ServiceWorker.Model exposing (..)


{-| Encodes a message in a way that our port can handle it.

So, obviously these could be simple strings in this case. But the general
pattern is

  - use a `tag` to give the Javascript side something easy to "switch" on
  - use additional fields to convey anything else needed

TODO: Now, these aren't all actually messages the port can handle ... I suppose
there really ought to be a subtype for those.

-}
encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        HandlePortMsg value ->
            object
                [ ( "tag", string "HandlePortMsg" )
                , ( "value", value )
                ]

        SetActive value ->
            object
                [ ( "tag", string "SetActive" )
                , ( "value", bool value )
                ]

        Register ->
            object
                [ ( "tag", string "Register" )
                ]

        Unregister ->
            object
                [ ( "tag", string "Unregister" )
                ]
