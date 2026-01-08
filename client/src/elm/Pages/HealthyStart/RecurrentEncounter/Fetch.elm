module Pages.HealthyStart.RecurrentEncounter.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.HealthyStart.Encounter.Fetch


fetch : HealthyStartEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.HealthyStart.Encounter.Fetch.fetch id db
