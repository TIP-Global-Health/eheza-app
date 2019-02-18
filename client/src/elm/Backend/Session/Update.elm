module Backend.Session.Update exposing (update)

import Backend.Endpoints exposing (sessionEndpoint)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Backend.Session.Encoder exposing (..)
import Backend.Session.Model exposing (..)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, withoutDecoder)


update : SessionId -> Msg -> Model -> ( Model, Cmd Msg )
update sessionId msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        CloseSession ->
            ( { model | closeSessionRequest = Loading }
            , object [ encodeClosed True ]
                |> sw.patchAny sessionEndpoint sessionId
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleClosedSession)
            )

        HandleClosedSession data ->
            ( { model | closeSessionRequest = data }
            , Cmd.none
            )

        MeasurementOutMsgChild childId subMsg ->
            ( model, Cmd.none )

        MeasurementOutMsgMother motherId subMsg ->
            ( model, Cmd.none )

        SetCheckedIn motherId checkedIn ->
            ( model, Cmd.none )
