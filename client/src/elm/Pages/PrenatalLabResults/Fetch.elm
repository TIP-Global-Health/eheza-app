module Pages.PrenatalLabResults.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.PrenatalEncounter.Fetch


fetch : PrenatalEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.PrenatalEncounter.Fetch.fetch id db
