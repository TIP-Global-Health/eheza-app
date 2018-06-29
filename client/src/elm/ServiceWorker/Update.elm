port module ServiceWorker.Update exposing (subscriptions, update)

{-| Interact with service workers.

The ports aren't exposed ... you have to interact with this
via sending messages through the `update` function.

-}

import Json.Decode exposing (Value, decodeValue)
import ServiceWorker.Decoder exposing (decodeMsg)
import ServiceWorker.Encoder exposing (encodeMsg)
import ServiceWorker.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandlePortMsg value ->
            case decodeValue decodeMsg value of
                Ok portMsg ->
                    update portMsg model

                Err err ->
                    let
                        _ =
                            Debug.log <|
                                "Error decoding message from port: "
                                    ++ err
                    in
                    ( model, Cmd.none )

        SetActive value ->
            ( { model | active = value }
            , Cmd.none
            )

        Register ->
            ( model
            , serviceWorkerOut (encodeMsg msg)
            )

        Unregister ->
            ( model
            , serviceWorkerOut (encodeMsg msg)
            )


subscriptions : Sub Msg
subscriptions =
    serviceWorkerIn HandlePortMsg


{-| Receive messages about service workers.
-}
port serviceWorkerIn : (Value -> msg) -> Sub msg


{-| Sends messages about service workers.
-}
port serviceWorkerOut : Value -> Cmd msg
