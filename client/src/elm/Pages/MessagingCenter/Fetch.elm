module Pages.MessagingCenter.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Wellbeing.Fetch


fetch : NominalDate -> NurseId -> ModelIndexedDb -> List MsgIndexedDb
fetch =
    Pages.Wellbeing.Fetch.fetch
