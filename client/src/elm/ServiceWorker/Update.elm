port module ServiceWorker.Update exposing (subscriptions, update)

{-| Interact with service workers.

The ports aren't exposed ... you have to interact with this
via sending messages through the `update` function.

-}

import App.Model
import Backend.Model
import Gizra.Update exposing (sequenceExtra)
import Json.Decode exposing (Value, decodeValue)
import Pages.Page
import RemoteData exposing (RemoteData(..))
import ServiceWorker.Decoder exposing (decodeIncomingMsg)
import ServiceWorker.Encoder exposing (encodeOutgoingMsg)
import ServiceWorker.Model exposing (..)
import Time exposing (Time)


update : Time -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentTime msg model =
    case msg of
        BackToPinCodePage ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage Pages.Page.PinCodePage ]
            )

        HandleIncomingMsg value ->
            case decodeValue decodeIncomingMsg value of
                Ok incoming ->
                    handleIncomingMsg currentTime incoming model

                Err err ->
                    let
                        _ =
                            Debug.log "decoder error" err
                    in
                    ( model, Cmd.none, [] )

        SendOutgoingMsg msg ->
            sendOutgoingMsg currentTime msg model


handleIncomingMsg : Time -> IncomingMsg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
handleIncomingMsg currentTime msg model =
    case msg of
        RegistrationSucceeded ->
            ( { model | registration = Success () }
            , Cmd.none
            , []
            )

        RegistrationFailed error ->
            ( { model | registration = Failure error }
            , Cmd.none
            , []
            )

        SetNewWorker worker ->
            let
                extraMsgs =
                    -- If we have a new worker that has finished installing,
                    -- and we're not controlled by anyone yet, then tell the
                    -- new worker to get on with it. Otherwise, we'll just
                    -- display the information in the UI and let the user
                    -- decide when to actually activate the new version.
                    if worker == Installed && not model.active then
                        [ SendOutgoingMsg SkipWaiting ]

                    else
                        []
            in
            ( { model | newWorker = Just worker }
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentTime) extraMsgs

        SetSyncData data ->
            ( model
            , Cmd.none
            , [ Success data
                    |> Backend.Model.HandleFetchedSyncData
                    |> App.Model.MsgIndexedDb
              ]
            )

        NewRevisions data ->
            ( model
            , Cmd.none
            , [ data
                    |> Backend.Model.HandleRevisions
                    |> App.Model.MsgIndexedDb
              ]
            )


sendOutgoingMsg : Time -> OutgoingMsg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
sendOutgoingMsg currentTime msg model =
    case msg of
        Register ->
            ( { model | registration = Loading }
            , serviceWorkerOut (encodeOutgoingMsg msg)
            , []
            )

        SkipWaiting ->
            ( model
            , serviceWorkerOut (encodeOutgoingMsg msg)
            , []
            )

        Update ->
            ( { model | lastUpdateCheck = Just currentTime }
            , serviceWorkerOut (encodeOutgoingMsg msg)
            , []
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
