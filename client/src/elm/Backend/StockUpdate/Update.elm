module Backend.ResilienceMessage.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.ResilienceMessage.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (encodeEntityUuid, toCmd, withoutDecoder)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update currentDate msg model =
    case msg of
        CreateStockUpdate stockUpdate ->
            createStockUpdate currentDate stockUpdate model

        HandleCreatedStockUpdate data ->
            ( { model | requestState = data }
            , Cmd.none
            )


createStockUpdate : NominalDate -> StockUpdate -> Model -> ( Model, Cmd Msg )
createStockUpdate currentDate stockUpdate model =
    ( { model | requestState = Loading }
    , sw.post stockUpdateEndpoint survey
        |> toCmd (RemoteData.fromResult >> HandleCreatedStockUpdate)
    )
