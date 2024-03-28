module Pages.HIV.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.HIV.Encounter.Fetch


fetch : HIVEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.HIV.Encounter.Fetch.fetch id db
