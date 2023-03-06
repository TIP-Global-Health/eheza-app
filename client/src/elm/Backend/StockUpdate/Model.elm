module Backend.StockUpdate.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PhotoUrl, StockUpdate)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { requestState : WebData ( StockUpdateId, StockUpdate ) }


emptyModel : Model
emptyModel =
    { requestState = NotAsked }


type Msg
    = CreateStockUpdate StockUpdate
    | HandleCreatedStockUpdate (WebData ( StockUpdateId, StockUpdate ))
