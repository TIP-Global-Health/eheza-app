module Pages.StockManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.StockManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId db =
    [ FetchStockManagementMeasurements healthCenterId
    , FetchStockManagementData healthCenterId
    ]
