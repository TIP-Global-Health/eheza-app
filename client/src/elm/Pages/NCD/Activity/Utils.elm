module Pages.NCD.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NCDEncounterId)
import Backend.Measurement.Model
    exposing
        ( NCDMeasurements
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.Utils exposing (taskCompleted)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
expectActivity currentDate assembled db activity =
    case activity of
        -- @todo
        _ ->
            False


activityCompleted : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
activityCompleted currentDate assembled db activity =
    case activity of
        -- @todo
        _ ->
            False
