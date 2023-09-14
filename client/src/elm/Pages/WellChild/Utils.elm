module Pages.WellChild.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementDateMeasuredFunc, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Measurement.Model exposing (VaccinationProgressDict)
import Measurement.Utils exposing (getPreviousMeasurements, mergeVaccinationProgressDicts)
import Pages.ChildScoreboard.Activity.Utils
import Pages.ChildScoreboard.Utils
import Pages.WellChild.Activity.Utils
import Pages.WellChild.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


generateVaccinationProgressDicts : AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDicts =
    Measurement.Utils.generateVaccinationProgressDictsForWellChild
