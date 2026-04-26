module Backend.StockUpdate.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (stockUpdateEndpoint)
import Backend.Measurement.Model exposing (StockUpdate)
import Backend.StockUpdate.Model exposing (Model, Msg(..))
import Backend.Utils exposing (sw)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CreateStockUpdate stockUpdate ->
            createStockUpdate stockUpdate model

        HandleCreatedStockUpdate data ->
            ( { model | requestState = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


createStockUpdate : StockUpdate -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
createStockUpdate record model =
    ( { model | requestState = Loading }
    , sw.post stockUpdateEndpoint record
        |> toCmd (RemoteData.fromResult >> HandleCreatedStockUpdate)
    , []
    )
