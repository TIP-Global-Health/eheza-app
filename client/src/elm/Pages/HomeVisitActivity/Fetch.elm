module Pages.HomeVisitActivity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.HomeVisitEncounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.HomeVisitEncounter.Fetch.fetch id db
