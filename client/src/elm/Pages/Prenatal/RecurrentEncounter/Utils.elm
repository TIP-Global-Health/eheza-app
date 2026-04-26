module Pages.Prenatal.RecurrentEncounter.Utils exposing (getAllActivities)

import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))


getAllActivities : Bool -> List PrenatalRecurrentActivity
getAllActivities isLabTech =
    if isLabTech then
        [ LabResults ]

    else
        [ RecurrentExamination, RecurrentMalariaPrevention, LabResults, LabsResultsFollowUps, RecurrentNextSteps ]
