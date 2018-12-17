module ServiceWorker.Decoder exposing (decodeIncomingMsg)

import Json.Decode exposing (..)
import ServiceWorker.Model exposing (..)


{-| Given some JSON our port sends in, decode it into a Msg we can handle. So,
the port can basically send any message we can handle here ... isn't that nice?
-}
decodeIncomingMsg : Decoder IncomingMsg
decodeIncomingMsg =
    field "tag" string
        |> andThen
            (\tag ->
                case tag of
                    "SetActive" ->
                        field "value" bool
                            |> map SetActive

                    "RegistrationSucceeded" ->
                        succeed RegistrationSucceeded

                    "RegistrationFailed" ->
                        field "error" value
                            |> map RegistrationFailed

                    _ ->
                        fail <|
                            "ServiceWorker.Decoder unrecognized tag: "
                                ++ tag
            )
