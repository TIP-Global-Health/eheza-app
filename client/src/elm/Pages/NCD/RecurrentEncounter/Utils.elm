module Pages.NCD.RecurrentEncounter.Utils exposing (allActivities)

import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))


allActivities : List NCDRecurrentActivity
allActivities =
    [ LabResults, RecurrentNextSteps ]
