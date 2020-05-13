module Pages.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.NutritionEncounter.Fetch exposing (fetchForChild)


fetch : PersonId -> ModelIndexedDb -> ( List MsgIndexedDb, List MsgIndexedDb )
fetch childId db =
    ( [ Backend.Model.FetchChildMeasurements childId
      , Backend.Model.FetchExpectedSessions childId
      ]
        ++ Pages.NutritionEncounter.Fetch.fetchForChild childId db
    , []
    )
