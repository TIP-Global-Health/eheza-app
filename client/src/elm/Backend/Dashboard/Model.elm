module Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, MaxValuePerType, Nutrition, NutritionStatus(..), NutritionValue, Periods, TotalBeneficiaries, emptyModel)

{-| The stats for the dashboard.
-}

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
import ZScore.Model exposing (ZScore)


type alias DashboardStats =
    { caseManagement : List CaseManagement
    , childrenBeneficiaries : List ChildrenBeneficiariesStats
    , familyPlanning : List FamilyPlanningStats
    , goodNutrition : GoodNutrition
    , malnourished : List MalnourishedStats
    , totalBeneficiaries : Dict Int TotalBeneficiaries
    , totalBeneficiariesMax : MaxValuePerType
    , totalEncounters : Periods
    }


emptyModel : DashboardStats
emptyModel =
    { caseManagement = []
    , childrenBeneficiaries = []
    , familyPlanning = []
    , goodNutrition =
        { all =
            { lastYear = 0
            , thisYear = 0
            }
        , good =
            { lastYear = 0
            , thisYear = 0
            }
        }
    , malnourished = []
    , totalEncounters =
        { lastYear = 0
        , thisYear = 0
        }
    , totalBeneficiaries = Dict.empty
    , totalBeneficiariesMax =
        { stunting = 0
        , underweight = 0
        , wasting = 0
        , muac = 0
        }
    }


type alias CaseManagement =
    { name : String
    , nutrition : CaseNutrition
    }


type alias CaseNutrition =
    { stunting : NutritionValue
    , underweight : NutritionValue
    , wasting : NutritionValue
    , muac : NutritionValue
    }


type alias ChildrenBeneficiariesStats =
    { gender : Gender
    , birthdate : NominalDate
    , memberSince : NominalDate
    }


type alias FamilyPlanningStats =
    { created : NominalDate
    , signs : List FamilyPlanningSign
    }


type alias MalnourishedStats =
    { created : NominalDate
    , gender : Gender
    , zscore : ZScore
    }


type alias Periods =
    { lastYear : Int
    , thisYear : Int
    }


type alias GoodNutrition =
    { all : Periods
    , good : Periods
    }


type alias Nutrition =
    { severeNutrition : Int
    , moderateNutrition : Int
    }


type alias NutritionValue =
    { class : NutritionStatus
    , value : String
    }


type NutritionStatus
    = Good
    | Moderate
    | Severe


type alias TotalBeneficiaries =
    { stunting : Nutrition
    , underweight : Nutrition
    , wasting : Nutrition
    , muac : Nutrition
    }


type alias MaxValuePerType =
    { stunting : Float
    , underweight : Float
    , wasting : Float
    , muac : Float
    }
