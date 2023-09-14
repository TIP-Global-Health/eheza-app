module Pages.HomeVisit.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.HomeVisit.Encounter.Fetch


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.HomeVisit.Encounter.Fetch.fetch id db
