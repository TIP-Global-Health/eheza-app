module Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, Nutrition, Periods, TotalBeneficiaries, emptyModel)

{-| The stats for the dashboard.
-}

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
import ZScore.Model exposing (ZScore)


type alias DashboardStats =
    { childrenBeneficiaries : List ChildrenBeneficiariesStats
    , familyPlanning : List FamilyPlanningStats
    , goodNutrition : GoodNutrition
    , malnourished : List MalnourishedStats
    , totalBeneficiaries : Dict Int TotalBeneficiaries
    , totalBeneficiariesMax : Float
    , totalEncounters : Periods
    }


emptyModel : DashboardStats
emptyModel =
    { childrenBeneficiaries = []
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
    , totalBeneficiariesMax = 0
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


type alias TotalBeneficiaries =
    { stunting : Nutrition
    , underweight : Nutrition
    , wasting : Nutrition
    , muac : Nutrition
    }
