module Pages.Dashboard.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.Dashboard.Model exposing (..)
import Pages.GlobalCaseManagement.Fetch


fetch : NominalDate -> HealthCenterId -> Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId maybeVillageId db =
    [ FetchVillages, FetchComputedDashboard healthCenterId ]
        ++ (case maybeVillageId of
                -- For CHW, we fetch case management data.
                Just villageId ->
                    Pages.GlobalCaseManagement.Fetch.fetch currentDate healthCenterId villageId db

                Nothing ->
                    []
           )
