module Pages.Prenatal.RecurrentEncounter.Utils exposing (..)

import Backend.PrenatalActivity.Model exposing (..)


getAllActivities : Bool -> List PrenatalRecurrentActivity
getAllActivities isLabTech =
    if isLabTech then
        [ LabResults ]

    else
        [ RecurrentExamination, RecurrentMalariaPrevention, LabResults, LabsResultsFollowUps, RecurrentNextSteps ]
