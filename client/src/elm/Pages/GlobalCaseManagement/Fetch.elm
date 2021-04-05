module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.GlobalCaseManagement.Model exposing (..)


fetch : HealthCenterId -> Model -> List MsgIndexedDb
fetch healthCenterId model =
    [ FetchVillages ]
