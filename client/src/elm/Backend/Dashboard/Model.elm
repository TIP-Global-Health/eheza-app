module Backend.Dashboard.Model exposing (ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, emptyModel)

{-| The stats for the dashboard.
-}

import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
import Time


type alias DashboardStats =
    { childrenBeneficiaries : List ChildrenBeneficiariesStats
    , familyPlanning : List FamilyPlanningStats
    }


emptyModel : DashboardStats
emptyModel =
    { childrenBeneficiaries = []
    , familyPlanning = []
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
