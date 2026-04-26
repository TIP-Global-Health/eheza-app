module Pages.StockManagement.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))
import Pages.StockManagement.Model exposing (StockManagementContext(..))


fetch : StockManagementContext -> List MsgIndexedDb
fetch context =
    case context of
        ContextHealthCenter healthCenterId ->
            [ FetchStockManagementMeasurements healthCenterId
            , FetchStockManagementData healthCenterId
            ]

        ContextVillage healthCenterId villageId ->
            [ FetchVillageStockManagementMeasurements healthCenterId
            , FetchVillageStockManagementData healthCenterId villageId
            , FetchPeopleInVillage villageId
            , FetchVillages
            ]
