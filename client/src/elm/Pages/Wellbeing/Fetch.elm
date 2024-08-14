module Pages.Wellbeing.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)


fetch : NominalDate -> NurseId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate nurseId db =
    [ FetchResilienceSurveysForNurse nurseId ]
