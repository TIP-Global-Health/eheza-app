module Pages.WellChild.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.WellChild.Encounter.Fetch


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.WellChild.Encounter.Fetch.fetch id db
