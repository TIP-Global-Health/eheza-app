module Pages.Prenatal.RecurrentActivity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.Prenatal.Encounter.Fetch


fetch : PrenatalEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.Prenatal.Encounter.Fetch.fetch id db
