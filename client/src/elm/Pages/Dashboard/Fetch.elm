module Pages.Dashboard.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Fetch


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch currentDate healthCenterId db model =
    [ FetchVillages, FetchComputedDashboard healthCenterId ]
        ++ (if isJust model.selectedVillageFilter then
                Pages.GlobalCaseManagement.Fetch.fetch currentDate healthCenterId model.selectedVillageFilter db

            else
                []
           )
