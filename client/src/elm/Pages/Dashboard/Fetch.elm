module Pages.Dashboard.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Fetch


fetch : NominalDate -> HealthCenterId -> VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId villageId db =
    FetchComputedDashboard healthCenterId
        :: Pages.GlobalCaseManagement.Fetch.fetch currentDate healthCenterId villageId db
