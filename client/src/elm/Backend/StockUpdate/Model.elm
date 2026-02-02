module Backend.StockUpdate.Model exposing (DataForMonth, Model, MonthYear, Msg(..), StockManagementData, emptyModel)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Fbf, StockUpdate)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { requestState : WebData ( StockUpdateId, StockUpdate ) }


emptyModel : Model
emptyModel =
    { requestState = NotAsked }


type alias StockManagementData =
    Dict MonthYear DataForMonth


type alias MonthYear =
    ( Int, Int )


type alias DataForMonth =
    { startingStock : Maybe Float
    , received : Float
    , issued : Float
    , currentBalance : Maybe Float
    , consumptionAverage : Float
    , stockUpdates : List StockUpdate
    , fbfs : List Fbf
    }


type Msg
    = CreateStockUpdate StockUpdate
    | HandleCreatedStockUpdate (WebData ( StockUpdateId, StockUpdate ))
