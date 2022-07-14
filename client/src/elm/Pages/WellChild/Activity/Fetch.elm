module Pages.WellChild.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.WellChild.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.WellChild.Encounter.Fetch.fetch id db
