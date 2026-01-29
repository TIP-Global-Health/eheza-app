module Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Measurement.Utils
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import RemoteData exposing (WebData)
import SyncManager.Model exposing (Site)


generateAssembledData : Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData =
    Measurement.Utils.generateAssembledDataForChildScoreboard
