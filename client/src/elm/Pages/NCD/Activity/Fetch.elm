module Pages.NCD.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.NCD.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : NCDEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.NCD.Encounter.Fetch.fetch id db
