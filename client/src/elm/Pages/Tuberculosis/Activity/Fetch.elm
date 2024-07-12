module Pages.Tuberculosis.Activity.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.Tuberculosis.Encounter.Fetch


fetch : TuberculosisEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    Pages.Tuberculosis.Encounter.Fetch.fetch id db
