module Backend.Scoreboard.Model exposing (..)

import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
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
    { ancNewborn : ANCNewbornData
    , universalIntervention : UniversalInterventionData
    , nutritionBehavior : NutritionBehaviorData
    , targetedInterventions : TargetedInterventionsData
    , infrastructureEnvironmentWash : InfrastructureEnvironmentWashData
    }


emptyNCDAData : NCDAData
emptyNCDAData =
    { ancNewborn = emptyANCNewbornData
    , universalIntervention = emptyUniversalInterventionData
    , nutritionBehavior = emptyNutritionBehaviorData
    , targetedInterventions = emptyTargetedInterventionsData
    , infrastructureEnvironmentWash = emptyInfrastructureEnvironmentWashData
    }


type alias ANCNewbornData =
    { row1 : Bool
    , row2 : Bool
    }


emptyANCNewbornData : ANCNewbornData
emptyANCNewbornData =
    ANCNewbornData False False


type alias UniversalInterventionData =
    { -- @todo
      row1 : VaccinationProgressDict
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate

    -- @todo
    , row5 : List NominalDate
    }


emptyUniversalInterventionData : UniversalInterventionData
emptyUniversalInterventionData =
    UniversalInterventionData Dict.empty [] [] [] []


type alias VaccinationProgressDict =
    Dict VaccineType (Dict VaccineDose NominalDate)


type alias RawVaccinationData =
    { bcg : EverySet NominalDate
    , opv : EverySet NominalDate
    , dtp : EverySet NominalDate
    , pcv13 : EverySet NominalDate
    , rotarix : EverySet NominalDate
    , ipv : EverySet NominalDate
    , mr : EverySet NominalDate
    }


type alias VaccinationValue =
    { administeredDoses : EverySet VaccineDose
    , administrationDates : EverySet NominalDate
    }


type VaccineType
    = VaccineBCG
    | VaccineOPV
    | VaccineDTP
    | VaccinePCV13
    | VaccineRotarix
    | VaccineIPV
    | VaccineMR


type VaccineDose
    = VaccineDoseFirst
    | VaccineDoseSecond
    | VaccineDoseThird
    | VaccineDoseFourth
    | VaccineDoseFifth


type alias NutritionBehaviorData =
    { row1 : List NominalDate
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate
    }


emptyNutritionBehaviorData : NutritionBehaviorData
emptyNutritionBehaviorData =
    NutritionBehaviorData [] [] [] []


type alias TargetedInterventionsData =
    { row1 : List NominalDate
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate
    , row5 : List NominalDate
    , row6 : List NominalDate
    }


emptyTargetedInterventionsData : TargetedInterventionsData
emptyTargetedInterventionsData =
    TargetedInterventionsData [] [] [] [] [] []


type alias InfrastructureEnvironmentWashData =
    { row1 : List NominalDate
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : Bool
    , row5 : List NominalDate
    }


emptyInfrastructureEnvironmentWashData : InfrastructureEnvironmentWashData
emptyInfrastructureEnvironmentWashData =
    InfrastructureEnvironmentWashData [] [] [] False []


type Msg
    = SetData Value
