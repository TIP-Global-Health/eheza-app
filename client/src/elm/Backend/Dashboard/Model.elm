module Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, ChildrenBeneficiariesStats, DashboardStats, FamilyPlanningStats, GoodNutrition, MalnourishedStats, Nutrition, NutritionStatus(..), NutritionValue, ParticipantStats, Periods, TotalBeneficiaries, emptyModel)

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
    , completedProgram : List ParticipantStats
    , completedProgramCount : Int
    , familyPlanning : List FamilyPlanningStats
    , maybeGoodNutrition : Maybe GoodNutrition
    , malnourished : List MalnourishedStats
    , missedSessions : List ParticipantStats
    , missedSessionsCount : Int
    , maybeTotalBeneficiaries : Maybe (Dict Int TotalBeneficiaries)
    , maybeTotalBeneficiariesIncidence : Maybe (Dict Int TotalBeneficiaries)
    , maybeTotalEncounters : Maybe Periods
    }


emptyModel : DashboardStats
emptyModel =
    { caseManagement = []
    , childrenBeneficiaries = []
    , completedProgram = []
    , completedProgramCount = 0
    , familyPlanning = []
    , maybeGoodNutrition = Nothing
    , malnourished = []
    , missedSessions = []
    , missedSessionsCount = 0
    , maybeTotalEncounters = Nothing
    , maybeTotalBeneficiaries = Nothing
    , maybeTotalBeneficiariesIncidence = Nothing
    }


type alias CaseManagement =
    { name : String
    , nutrition : CaseNutrition
    }


type alias CaseNutrition =
    { stunting : Dict Int NutritionValue
    , underweight : Dict Int NutritionValue
    , wasting : Dict Int NutritionValue
    , muac : Dict Int NutritionValue
    }


type alias ChildrenBeneficiariesStats =
    { gender : Gender
    , birthDate : NominalDate
    , memberSince : NominalDate
    , name : String
    , motherName : String
    , phoneNumber : Maybe String
    }


type alias FamilyPlanningStats =
    { created : NominalDate
    , signs : List FamilyPlanningSign
    }


type alias MalnourishedStats =
    { created : NominalDate
    , birthDate : NominalDate
    , gender : Gender
    , zscore : ZScore
    }


type alias ParticipantStats =
    { name : String
    , gender : Gender
    , birthDate : NominalDate
    , motherName : String
    , phoneNumber : Maybe String
    , dates : List NominalDate
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


type alias TotalBeneficiaries =
    { stunting : Nutrition
    , underweight : Nutrition
    , wasting : Nutrition
    , muac : Nutrition
    }


type NutritionStatus
    = Good
    | Moderate
    | Neutral
    | Severe
