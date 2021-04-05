module Pages.GlobalCaseManagement.Utils exposing (..)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Pages.GlobalCaseManagement.Model exposing (..)


allEncounterTypes : List IndividualEncounterType
allEncounterTypes =
    [ AcuteIllnessEncounter, NutritionEncounter ]
