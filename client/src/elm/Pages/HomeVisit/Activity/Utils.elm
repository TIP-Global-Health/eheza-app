module Pages.HomeVisit.Activity.Utils exposing (activityCompleted, expectActivity)

import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Maybe.Extra exposing (isJust)
import Pages.HomeVisit.Encounter.Model exposing (AssembledData)


expectActivity : HomeVisitActivity -> Bool
expectActivity _ =
    -- For now, we show all activities without any conditions.
    True


activityCompleted : AssembledData -> HomeVisitActivity -> Bool
activityCompleted data activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        Feeding ->
            (not <| expectActivity Feeding)
                || isJust measurements.feeding

        Caring ->
            (not <| expectActivity Caring)
                || isJust measurements.caring

        Hygiene ->
            (not <| expectActivity Hygiene)
                || isJust measurements.hygiene

        FoodSecurity ->
            (not <| expectActivity FoodSecurity)
                || isJust measurements.foodSecurity
