module Pages.ChildScoreboard.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Measurement.Utils
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.ChildScoreboard.Utils exposing (generatePreviousMeasurements, generateVaccinationProgressDicts)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (Site)


generateAssembledData : Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData =
    Measurement.Utils.generateAssembledDataForChildScoreboard
