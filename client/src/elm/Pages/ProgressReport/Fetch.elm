module Pages.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Backend.NutritionEncounter.Fetch exposing (fetch)


fetch : PersonId -> ModelIndexedDb -> ( List MsgIndexedDb, List MsgIndexedDb )
fetch childId db =
    ( Backend.Model.FetchExpectedSessions childId
        :: Backend.NutritionEncounter.Fetch.fetch childId db
    , []
    )
