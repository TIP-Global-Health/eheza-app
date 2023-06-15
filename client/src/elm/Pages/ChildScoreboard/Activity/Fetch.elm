module Pages.ChildScoreboard.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.ChildScoreboard.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : ChildScoreboardEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.ChildScoreboard.Encounter.Fetch.fetch id db
