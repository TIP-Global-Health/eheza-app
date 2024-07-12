module Backend.StockUpdate.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Measurement.Model exposing (StockUpdate)
import Backend.StockUpdate.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        CreateStockUpdate stockUpdate ->
            createStockUpdate currentDate stockUpdate model

        HandleCreatedStockUpdate data ->
            ( { model | requestState = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


createStockUpdate : NominalDate -> StockUpdate -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
createStockUpdate currentDate record model =
    ( { model | requestState = Loading }
    , sw.post stockUpdateEndpoint record
        |> toCmd (RemoteData.fromResult >> HandleCreatedStockUpdate)
    , []
    )
