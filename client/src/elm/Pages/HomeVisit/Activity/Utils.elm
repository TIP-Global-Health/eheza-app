module Pages.HomeVisit.Activity.Utils exposing (activityCompleted, expectActivity)

import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.HomeVisit.Encounter.Model exposing (AssembledData)


expectActivity : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
expectActivity currentDate child data db activity =
    -- For now, we show all activities without any conditions.
    True


activityCompleted : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
activityCompleted currentDate child data db activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        Feeding ->
            (not <| expectActivity currentDate child data db Feeding)
                || isJust measurements.feeding

        Caring ->
            (not <| expectActivity currentDate child data db Caring)
                || isJust measurements.caring

        Hygiene ->
            (not <| expectActivity currentDate child data db Hygiene)
                || isJust measurements.hygiene

        FoodSecurity ->
            (not <| expectActivity currentDate child data db FoodSecurity)
                || isJust measurements.foodSecurity
