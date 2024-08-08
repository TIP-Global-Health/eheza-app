module Pages.Tuberculosis.ProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.Tuberculosis.Encounter.Fetch


fetch : TuberculosisEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.Tuberculosis.Encounter.Fetch.fetch
