module ServiceWorker.Decoder exposing (decodeIncomingMsg)

import Backend.Decoder exposing (decodeRevision)
import Json.Decode exposing (Decoder, andThen, fail, field, list, map, string, succeed)
import ServiceWorker.Model exposing (IncomingMsg(..), NewWorker(..))


{-| Given some JSON our port sends in, decode it into a Msg we can handle. So,
the port can basically send any message we can handle here ... isn't that nice?
-}
decodeIncomingMsg : Decoder IncomingMsg
decodeIncomingMsg =
    field "tag" string
        |> andThen
            (\tag ->
                case tag of
                    "RegistrationSucceeded" ->
                        succeed RegistrationSucceeded

                    "RegistrationFailed" ->
                        field "error" string
                            |> map RegistrationFailed

                    "SetNewWorker" ->
                        field "state" decodeNewWorker
                            |> map SetNewWorker

                    "NewRevisions" ->
                        field "data" (list decodeRevision)
                            |> map NewRevisions

                    _ ->
                        fail <|
                            "ServiceWorker.Decoder unrecognized tag: "
                                ++ tag
            )


decodeNewWorker : Decoder NewWorker
decodeNewWorker =
    string
        |> andThen
            (\s ->
                case s of
                    "installing" ->
                        succeed Installing

                    "installed" ->
                        succeed Installed

                    "activating" ->
                        succeed Activating

                    "activated" ->
                        succeed Activated

                    "redundant" ->
                        succeed Redundant

                    _ ->
                        fail <|
                            "Unrecognized NewWorker state: "
                                ++ s
            )
