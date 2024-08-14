module Pages.StockManagement.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId db =
    [ FetchStockManagementMeasurements healthCenterId
    , FetchStockManagementData healthCenterId
    ]
