module Pages.StockManagement.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.StockManagement.Model exposing (StockManagementContext(..))


fetch : NominalDate -> StockManagementContext -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate context db =
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
