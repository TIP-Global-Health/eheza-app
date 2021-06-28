module Pages.WellChildActivity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.WellChildEncounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.WellChildEncounter.Fetch.fetch id db
