module Pages.WellChildProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.WellChildEncounter.Fetch


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.WellChildEncounter.Fetch.fetch
