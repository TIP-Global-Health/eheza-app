module Pages.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Pages.AcuteIllness.Participant.Fetch


fetch : PersonId -> ModelIndexedDb -> ( List MsgIndexedDb, List MsgIndexedDb )
fetch childId db =
    -- See Pages.Nutrition.ProgressReport.Fetch: the Next Appointment pane needs
    -- the health centers, and nothing else on this page fetches them.
    ( (FetchHealthCenters
        :: Backend.Model.FetchExpectedSessions childId
        :: Backend.NutritionEncounter.Fetch.fetch childId db
      )
        ++ Pages.AcuteIllness.Participant.Fetch.fetch childId db
    , []
    )
