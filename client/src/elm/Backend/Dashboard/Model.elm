module Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, MalnourishedStats, emptyModel)

{-| The stats for the dashboard.
-}

import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
import Time
import ZScore.Model exposing (ZScore)


type alias DashboardStats =
    { childrenBeneficiaries : List ChildrenBeneficiariesStats
    , familyPlanning : List FamilyPlanningStats
    , malnourished : List MalnourishedStats
    }


emptyModel : DashboardStats
emptyModel =
    { childrenBeneficiaries = []
    , familyPlanning = []
    , malnourished = []
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
