module Pages.NutritionActivity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.NutritionEncounter.Fetch


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.NutritionEncounter.Fetch.fetch id db
