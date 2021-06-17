module Pages.WellChildActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (WellChildEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, ifTrue, taskCompleted)
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
expectActivity currentDate child data db activity =
    -- For now, we show all activities without any conditions.
    case activity of
        WellChildECD ->
            True

        _ ->
            False


activityCompleted : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
activityCompleted currentDate child data db activity =
    let
        measurements =
            data.measurements

        activityExpected =
            expectActivity currentDate child data db
    in
    case activity of
        WellChildNutritionAssessment ->
            (not <| activityExpected WellChildNutritionAssessment) || False

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd
