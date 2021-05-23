module Backend.Dashboard.Model exposing (..)

{-| The stats for the dashboard.
-}

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)


{-| To void a cycle in dependency, we just define the zScore here.
Added a comment in the main definition to point to this one.
-}
type alias ZScore =
    Float


type alias DashboardStats =
    { caseManagement : CaseManagementData
    , childrenBeneficiaries : List ChildrenBeneficiariesStats
    , completedPrograms : List ParticipantStats
    , familyPlanning : List FamilyPlanningStats
    , missedSessions : List ParticipantStats
    , totalEncounters : TotalEncountersData
    , prenatalData : List PrenatalDataItem
    , villagesWithResidents : Dict VillageId (List PersonIdentifier)

    -- UTC Date and time on which statistics were generated.
    , timestamp : String

    -- An md5 hash, using which we know if we have the most up to date data.
    , cacheHash : String
    }


emptyModel : DashboardStats
emptyModel =
    { caseManagement = CaseManagementData Dict.empty Dict.empty
    , childrenBeneficiaries = []
    , completedPrograms = []
    , familyPlanning = []
    , missedSessions = []
    , totalEncounters = TotalEncountersData Dict.empty Dict.empty
    , prenatalData = []
    , villagesWithResidents = Dict.empty
    , timestamp = ""
    , cacheHash = ""
    }


type alias PersonIdentifier =
    Int


type alias CaseManagementData =
    { thisYear : Dict ProgramType (List CaseManagement)
    , lastYear : Dict ProgramType (List CaseManagement)
    }


type alias CaseManagement =
    { identifier : PersonIdentifier
    , name : String
    , birthDate : NominalDate
    , gender : Gender
    , nutrition : CaseNutrition
    }


type alias CaseNutrition =
    { stunting : Dict Int NutritionValue
    , underweight : Dict Int NutritionValue
    , wasting : Dict Int NutritionValue
    , muac : Dict Int NutritionValue
    , nutritionSigns : Dict Int NutritionValue
    }


type alias CaseNutritionTotal =
    { stunting : Dict Int Nutrition
    , underweight : Dict Int Nutrition
    , wasting : Dict Int Nutrition
    , muac : Dict Int Nutrition
    , nutritionSigns : Dict Int Nutrition
    }


type alias ChildrenBeneficiariesStats =
    { gender : Gender
    , birthDate : NominalDate
    , memberSince : NominalDate
    , name : String
    , motherName : String
    , phoneNumber : Maybe String
    , graduationDate : NominalDate
    }


type alias FamilyPlanningStats =
    { created : NominalDate
    , signs : List FamilyPlanningSign
    }


type alias ParticipantStats =
    { name : String
    , gender : Gender
    , birthDate : NominalDate
    , motherName : String
    , phoneNumber : Maybe String
    , expectedDate : NominalDate
    }


type alias Periods =
    { lastYear : Int
    , thisYear : Int
    }


type alias Nutrition =
    { severeNutrition : Int
    , moderateNutrition : Int
    }


emptyNutrition : Nutrition
emptyNutrition =
    Nutrition 0 0


type alias NutritionValue =
    { class : NutritionStatus
    , value : String
    }


emptyNutritionValue : NutritionValue
emptyNutritionValue =
    { class = Neutral
    , value = "X"
    }


type alias TotalBeneficiaries =
    { stunting : Nutrition
    , underweight : Nutrition
    , wasting : Nutrition
    , muac : Nutrition
    }


emptyTotalBeneficiaries : TotalBeneficiaries
emptyTotalBeneficiaries =
    { stunting = emptyNutrition
    , underweight = emptyNutrition
    , wasting = emptyNutrition
    , muac = emptyNutrition
    }


type NutritionStatus
    = Good
    | Moderate
    | Neutral
    | Severe


type ProgramType
    = ProgramAchi
    | ProgramFbf
    | ProgramIndividual
    | ProgramPmtct
    | ProgramSorwathe
    | ProgramUnknown


type alias TotalEncountersData =
    { global : Dict ProgramType Periods
    , villages : Dict VillageId (Dict ProgramType Periods)
    }


type alias PrenatalDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , expectedDateConcluded : Maybe NominalDate
    , dateConcluded : Maybe NominalDate
    , outcome : Maybe IndividualEncounterParticipantOutcome
    , deliveryLocation : Maybe DeliveryLocation
    }
