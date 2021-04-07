module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.GlobalCaseManagement.Model exposing (..)


fetch : HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch healthCenterId db =
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    ]
