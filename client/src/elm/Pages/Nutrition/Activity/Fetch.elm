module Pages.Nutrition.Activity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Maybe.Extra
import Pages.Nutrition.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.Nutrition.Encounter.Fetch.fetch id db
