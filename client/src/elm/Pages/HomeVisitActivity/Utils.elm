module Pages.HomeVisitActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (HomeVisitEncounterId)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.HomeVisitActivity.Model exposing (..)
import Pages.HomeVisitEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, taskCompleted, valueConsideringIsDirtyField)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> Person -> Bool -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
expectActivity currentDate child isChw data db activity =
    -- @todo
    case activity of
        _ ->
            True


activityCompleted : NominalDate -> Person -> Bool -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
activityCompleted currentDate child isChw data db activity =
    -- @todo
    case activity of
        _ ->
            True
