module Pages.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)


fetch : ChildId -> List MsgIndexedDb
fetch childId =
    [ Backend.Model.FetchChildMeasurements childId
    , Backend.Model.FetchExpectedSessions childId
    ]
