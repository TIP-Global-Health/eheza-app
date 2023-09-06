module Pages.Prenatal.RecurrentEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getHeightValue, getMeasurementValueFunc, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, orElse, unwrap)
import Pages.Prenatal.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


allActivities : List PrenatalRecurrentActivity
allActivities =
    [ RecurrentExamination, RecurrentMalariaPrevention, LabResults, RecurrentNextSteps ]
