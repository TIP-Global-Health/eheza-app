module Backend.Scoreboard.Model exposing (..)

import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias ScoreboardData =
    { entityName : String
    , entityType : SelectedEntity
    , records : List PatientData
    }


type SelectedEntity
    = EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage


type alias PatientData =
    { birthDate : NominalDate
    , lowBirthWeight : Maybe Bool
    , nutrition : NutritionCriterionsData
    , ncda : NCDAData
    }


type alias NutritionCriterionsData =
    { stunting : CriterionBySeverities
    , underweight : CriterionBySeverities
    , wasting : CriterionBySeverities
    , muac : CriterionBySeverities
    }


emptyNutritionCriterionsData : NutritionCriterionsData
emptyNutritionCriterionsData =
    { stunting = emptyCriterionBySeverities
    , underweight = emptyCriterionBySeverities
    , wasting = emptyCriterionBySeverities
    , muac = emptyCriterionBySeverities
    }


type alias CriterionBySeverities =
    { severe : List NominalDate
    , moderate : List NominalDate
    , normal : List NominalDate
    }


emptyCriterionBySeverities : CriterionBySeverities
emptyCriterionBySeverities =
    { severe = []
    , moderate = []
    , normal = []
    }


type alias NCDAData =
    { postpartumCheckups : Bool
    , ironDuringPregnancy : Bool
    , nutritionBehavior : NutritionBehaviorData
    , infrastructureEnvironmentWash : InfrastructureEnvironmentWashData
    }


emptyNCDAData : NCDAData
emptyNCDAData =
    { postpartumCheckups = False
    , ironDuringPregnancy = False
    , nutritionBehavior = emptyNutritionBehaviorData
    , infrastructureEnvironmentWash = emptyInfrastructureEnvironmentWashData
    }


type alias NutritionBehaviorData =
    { row1 : List NominalDate
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate
    }


emptyNutritionBehaviorData : NutritionBehaviorData
emptyNutritionBehaviorData =
    NutritionBehaviorData [] [] [] []


type alias InfrastructureEnvironmentWashData =
    { row1 : List NominalDate
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate
    , row5 : List NominalDate
    }


emptyInfrastructureEnvironmentWashData : InfrastructureEnvironmentWashData
emptyInfrastructureEnvironmentWashData =
    InfrastructureEnvironmentWashData [] [] [] [] []


type Msg
    = SetData Value
