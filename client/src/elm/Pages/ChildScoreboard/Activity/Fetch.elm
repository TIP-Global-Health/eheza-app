module Pages.ChildScoreboard.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.ChildScoreboard.Encounter.Fetch


fetch : ChildScoreboardEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.ChildScoreboard.Encounter.Fetch.fetch id db
