port module ServiceWorker.Update exposing (subscriptions, update)

{-| Interact with service workers.

The ports aren't exposed ... you have to interact with this
via sending messages through the `update` function.

-}

import Json.Decode exposing (Value, decodeValue)
import ServiceWorker.Decoder exposing (decodeIncomingMsg)
import ServiceWorker.Encoder exposing (encodeOutgoingMsg)
import ServiceWorker.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleIncomingMsg value ->
            case decodeValue decodeIncomingMsg value of
                Ok incoming ->
                    handleIncomingMsg incoming model

                Err err ->
                    let
                        _ =
                            Debug.log <|
                                "Error decoding message from port: "
                                    ++ err
                    in
                    ( model, Cmd.none )

        SendOutgoingMsg msg ->
            sendOutgoingMsg msg model


handleIncomingMsg : IncomingMsg -> Model -> ( Model, Cmd Msg )
handleIncomingMsg msg model =
    case msg of
        SetActive value ->
            ( { model | active = value }
            , Cmd.none
            )


sendOutgoingMsg : OutgoingMsg -> Model -> ( Model, Cmd Msg )
sendOutgoingMsg msg model =
    -- At some point, we may need to distinguish further among them.
    -- But for now we can just encode.
    ( model
    , serviceWorkerOut (encodeOutgoingMsg msg)
    )


subscriptions : Sub Msg
subscriptions =
    serviceWorkerIn HandleIncomingMsg


{-| Receive messages about service workers.
-}
port serviceWorkerIn : (Value -> msg) -> Sub msg


{-| Sends messages about service workers.
-}
port serviceWorkerOut : Value -> Cmd msg
