module Pages.Prenatal.RecurrentEncounter.Utils exposing (..)

import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isLabTechnician)
import Backend.PrenatalActivity.Model exposing (..)


getAllActivities : Nurse -> List PrenatalRecurrentActivity
getAllActivities nurse =
    if isLabTechnician nurse then
        [ LabResults ]

    else
        [ RecurrentExamination, RecurrentMalariaPrevention, LabResults, RecurrentNextSteps ]
