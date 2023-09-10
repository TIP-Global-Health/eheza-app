module Pages.Prenatal.RecurrentEncounter.Utils exposing (allActivities)

import Backend.PrenatalActivity.Model exposing (..)


allActivities : List PrenatalRecurrentActivity
allActivities =
    [ RecurrentExamination, RecurrentMalariaPrevention, LabResults, RecurrentNextSteps ]
