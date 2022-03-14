module Pages.AcuteIllness.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.AcuteIllness.Encounter.Fetch


fetch : AcuteIllnessEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.AcuteIllness.Encounter.Fetch.fetch
