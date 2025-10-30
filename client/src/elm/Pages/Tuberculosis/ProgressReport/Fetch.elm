module Pages.Tuberculosis.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.Tuberculosis.Encounter.Fetch


fetch : TuberculosisEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.Tuberculosis.Encounter.Fetch.fetch
