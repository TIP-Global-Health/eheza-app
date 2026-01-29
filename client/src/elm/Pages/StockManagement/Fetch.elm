module Pages.StockManagement.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : HealthCenterId -> List MsgIndexedDb
fetch healthCenterId =
    [ FetchStockManagementMeasurements healthCenterId
    , FetchStockManagementData healthCenterId
    ]
