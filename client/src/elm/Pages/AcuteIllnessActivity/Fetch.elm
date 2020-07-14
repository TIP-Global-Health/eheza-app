module Pages.AcuteIllnessActivity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.AcuteIllnessEncounter.Fetch


fetch : AcuteIllnessEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.AcuteIllnessEncounter.Fetch.fetch id db
