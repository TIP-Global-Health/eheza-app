module Pages.HomeVisit.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.HomeVisit.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.HomeVisit.Encounter.Fetch.fetch id db
