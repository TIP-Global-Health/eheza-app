module ServiceWorker.Decoder exposing (..)

import Json.Decode exposing (..)
import ServiceWorker.Model exposing (..)


{-| Given some JSON our port sends in, decode it into a Msg we can handle. So,
the port can basically send any message we can handle here ... isn't that nice?
-}
decodeMsg : Decoder Msg
decodeMsg =
    field "tag" string
        |> andThen
            (\tag ->
                case tag of
                    "SetActive" ->
                        field "value" bool
                            |> map SetActive

                    _ ->
                        fail <|
                            "ServiceWorker.Decoder unrecognized tag: "
                                ++ tag
            )
