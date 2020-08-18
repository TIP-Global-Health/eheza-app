module Pages.AcuteIllnessProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.AcuteIllnessEncounter.Fetch


fetch : AcuteIllnessEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.AcuteIllnessEncounter.Fetch.fetch
