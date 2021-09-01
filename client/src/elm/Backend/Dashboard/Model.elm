module Backend.Dashboard.Model exposing (..)

{-| The stats for the dashboard.
-}

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome)
import Backend.Measurement.Model
    exposing
        ( Call114Sign
        , DangerSign
        , FamilyPlanningSign
        , FollowUpMeasurements
        , HCContactSign
        , HCRecommendation
        , IsolationSign
        , Recommendation114
        , SendToHCSign
        )
import Backend.Person.Model exposing (Gender)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)


type alias AssembledData =
    { stats : DashboardStats
    , acuteIllnessData : List AcuteIllnessDataItem
    , prenatalData : List PrenatalDataItem
    , caseManagementData : Maybe FollowUpMeasurements
    , nutritionPageData : NutritionPageData
    }


type alias NutritionPageData =
    { caseNutritionTotalsLastYear : List CaseNutritionTotal
    , caseNutritionTotalsThisYear : List CaseNutritionTotal
    , totalEncounters : Periods
    , newCasesGraphData : Dict Int TotalBeneficiaries
    , totalsGraphData : Dict Int TotalBeneficiaries
    }


{-| To void a cycle in dependency, we just define the zScore here.
Added a comment in the main definition to point to this one.
-}
type alias ZScore =
    Float


type alias DashboardStatsRaw =
    { caseManagement : CaseManagementData
    , childrenBeneficiaries : ChildrenBeneficiariesData
    , completedPrograms : List ParticipantStats
    , familyPlanning : List FamilyPlanningStats
    , missedSessions : List ParticipantStats
    , totalEncounters : TotalEncountersData
    , acuteIllnessData : List AcuteIllnessDataItem
    , prenatalData : List PrenatalDataItem
    , villagesWithResidents : Dict VillageId (List PersonIdentifier)

    -- UTC Date and time on which statistics were generated.
    , timestamp : String

    -- An md5 hash, using which we know if we have the most up to date data.
    , cacheHash : String
    }


emptyModel : DashboardStatsRaw
emptyModel =
    { caseManagement = CaseManagementData Dict.empty Dict.empty
    , childrenBeneficiaries = Dict.empty
    , completedPrograms = []
    , familyPlanning = []
    , missedSessions = []
    , totalEncounters = TotalEncountersData Dict.empty Dict.empty
    , acuteIllnessData = []
    , prenatalData = []
    , villagesWithResidents = Dict.empty
    , timestamp = ""
    , cacheHash = ""
    }


type alias DashboardStats =
    { caseManagement : CaseManagementPast2Years
    , childrenBeneficiaries : List ChildrenBeneficiariesStats
    , completedPrograms : List ParticipantStats
    , familyPlanning : List FamilyPlanningStats
    , missedSessions : List ParticipantStats
    , totalEncounters : TotalEncountersData
    , villagesWithResidents : Dict VillageId (List PersonIdentifier)

    -- UTC Date and time on which statistics were generated.
    , timestamp : String
    }


type alias CaseManagementPast2Years =
    { thisYear : List CaseManagement
    , lastYear : List CaseManagement
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


type alias ChildrenBeneficiariesData =
    Dict ProgramType (List ChildrenBeneficiariesStats)


type alias ChildrenBeneficiariesStats =
    { identifier : PersonIdentifier
    , gender : Gender
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
    | ProgramChw
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
    , encounters : List PrenatalEncounterDataItem
    }


type alias PrenatalEncounterDataItem =
    { startDate : NominalDate
    , dangerSigns : EverySet DangerSign
    }


type alias AcuteIllnessDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , diagnosis : AcuteIllnessDiagnosis
    , dateConcluded : Maybe NominalDate
    , outcome : Maybe IndividualEncounterParticipantOutcome
    , encounters : List AcuteIllnessEncounterDataItem
    }


type alias AcuteIllnessEncounterDataItem =
    { startDate : NominalDate
    , sequenceNumber : Int
    , diagnosis : AcuteIllnessDiagnosis
    , feverRecorded : Bool
    , isolationSigns : EverySet IsolationSign
    , sendToHCSigns : EverySet SendToHCSign
    , call114Signs : EverySet Call114Sign
    , recommendation114 : EverySet Recommendation114
    , hcContactSigns : EverySet HCContactSign
    , hcRecommendation : EverySet HCRecommendation
    }
