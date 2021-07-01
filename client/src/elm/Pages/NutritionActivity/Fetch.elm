module Pages.NutritionActivity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.NutritionEncounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.NutritionEncounter.Fetch.fetch id db
