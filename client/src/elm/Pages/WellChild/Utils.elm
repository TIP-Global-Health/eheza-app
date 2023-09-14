module Pages.WellChild.Utils exposing (..)

import Backend.Model exposing (ModelIndexedDb)
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils
import Pages.WellChild.Encounter.Model exposing (..)


generateVaccinationProgressDicts : AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDicts =
    Measurement.Utils.generateVaccinationProgressDictsForWellChild
