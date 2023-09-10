module Pages.NCD.RecurrentEncounter.Utils exposing (allActivities)

import Backend.NCDActivity.Model exposing (..)


allActivities : List NCDRecurrentActivity
allActivities =
    [ LabResults, RecurrentNextSteps ]
