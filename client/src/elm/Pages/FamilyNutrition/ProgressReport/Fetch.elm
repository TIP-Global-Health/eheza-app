module Pages.FamilyNutrition.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.FamilyNutrition.Encounter.Fetch


fetch : FamilyNutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.FamilyNutrition.Encounter.Fetch.fetch
