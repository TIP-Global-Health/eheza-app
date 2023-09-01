module Backend.ResilienceMessage.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.ResilienceMessage.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (encodeEntityUuid, toCmd, withoutDecoder)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        UpdateMessage messageId message ->
            updateMessage currentDate messageId message model

        HandleUpdatedMessage data ->
            ( { model | updateMessage = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateMessage : NominalDate -> ResilienceMessageId -> ResilienceMessage -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
updateMessage currentDate messageId message model =
    ( { model | updateMessage = Loading }
    , sw.patchFull resilienceMessageEndpoint messageId message
        |> withoutDecoder
        |> toCmd (RemoteData.fromResult >> HandleUpdatedMessage)
    , []
    )
