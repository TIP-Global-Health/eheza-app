module Pages.Dashboard.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.Dashboard.Model exposing (..)


fetch : HealthCenterId -> Model -> List MsgIndexedDb
fetch healthCenterId model =
    [ FetchVillages, FetchComputedDashboard healthCenterId ]
