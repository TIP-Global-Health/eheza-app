module Pages.Nutrition.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.Nutrition.Encounter.Fetch


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.Nutrition.Encounter.Fetch.fetch id db
