module Backend.Completion.Model exposing (..)

import App.Types exposing (Site)
import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias CompletionData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    , nutritionIndividualData : List (EncounterData NutritionChildActivity)
    , nutritionGroupData : List (NutritionGroupEncounterData NutritionMotherActivity NutritionChildActivity)
    }


type SelectedEntity
    = EntityGlobal
    | EntityHealthCenter


type alias EncounterData activity =
    { startDate : NominalDate
    , expectedActivities : List activity
    , completedActivities : List activity
    , takenBy : Maybe TakenBy
    }


type alias NutritionGroupEncounterData motherActivity childActivity =
    { startDate : NominalDate
    , motherData : Maybe (ActivitiesCompletionData motherActivity)
    , childrenData : List (ActivitiesCompletionData childActivity)
    , takenBy : Maybe TakenBy
    }


type alias ActivitiesCompletionData activity =
    { expectedActivities : List activity
    , completedActivities : List activity
    }


type NutritionChildActivity
    = NutritionHeight
    | NutritionNutrition
    | NutritionPhoto
    | NutritionWeight
    | NutritionMUAC
    | NutritionContributingFactors
    | NutritionFollowUp
    | NutritionHealthEducation
    | NutritionSendToHC
    | NutritionNCDA
    | NutritionChildFbf


type NutritionMotherActivity
    = NutritionFamilyPlanning
    | NutritionLactation
    | NutritionMotherFbf


type TakenBy
    = TakenByNurse
    | TakenByCHW
    | TakenByUnknown


type Msg
    = SetData Value
