module Backend.Dashboard.Model exposing (..)

{-| The stats for the dashboard.
-}

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis, AcuteIllnessEncounterType)
import Backend.Entities exposing (VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation, IndividualEncounterParticipantOutcome)
import Backend.Measurement.Model
    exposing
        ( Call114Sign
        , ChildNutritionSign
        , DangerSign
        , FamilyPlanningSign
        , Gender
        , HCContactSign
        , HCRecommendation
        , IsolationSign
        , MedicalCondition
        , Recommendation114
        , SendToHCSign
        , TestExecutionNote
        , TestResult
        )
import Backend.NCDEncounter.Types exposing (NCDDiagnosis)
import Backend.NutritionEncounter.Model exposing (NutritionEncounterType)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import Backend.WellChildEncounter.Model exposing (EncounterWarning, WellChildEncounterType)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)


type alias AssembledData =
    { stats : DashboardStats
    , acuteIllnessData : List AcuteIllnessDataItem
    , prenatalData : List PrenatalDataItem
    , ncdData : List NCDDataItem
    , pmtctData : List PMTCTDataItem
    , spvData : List SPVDataItem
    , childScoreboardData : List ChildScoreboardDataItem
    , nutritionIndividualData : List NutritionIndividualDataItem
    , nutritionGroupData : List NutritionGroupDataItem
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
    , ncdData : List NCDDataItem
    , pmtctData : List PMTCTDataItem
    , spvData : List SPVDataItem
    , childScoreboardData : List ChildScoreboardDataItem
    , nutritionIndividualData : List NutritionIndividualDataItem
    , nutritionGroupData : List NutritionGroupDataItem
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
    , ncdData = []
    , pmtctData = []
    , spvData = []
    , childScoreboardData = []
    , nutritionIndividualData = []
    , nutritionGroupData = []
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
    , encounterType : PrenatalEncounterType
    , dangerSigns : EverySet DangerSign
    , diagnoses : EverySet PrenatalDiagnosis
    , muac : Maybe Float
    , sendToHCSigns : EverySet SendToHCSign
    }


type alias AcuteIllnessDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , birthDate : NominalDate
    , diagnosis : AcuteIllnessDiagnosis
    , dateConcluded : Maybe NominalDate
    , outcome : Maybe IndividualEncounterParticipantOutcome
    , encounters : List AcuteIllnessEncounterDataItem
    }


type alias AcuteIllnessEncounterDataItem =
    { startDate : NominalDate
    , encounterType : AcuteIllnessEncounterType
    , sequenceNumber : Int
    , ageInMonths : Int
    , diagnosis : AcuteIllnessDiagnosis
    , feverRecorded : Bool
    , isolationSigns : EverySet IsolationSign
    , sendToHCSigns : EverySet SendToHCSign
    , call114Signs : EverySet Call114Sign
    , recommendation114 : EverySet Recommendation114
    , hcContactSigns : EverySet HCContactSign
    , hcRecommendation : EverySet HCRecommendation
    }


type alias NCDDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , birthDate : NominalDate
    , encounters : List NCDEncounterDataItem
    }


type alias NCDEncounterDataItem =
    { startDate : NominalDate
    , diagnoses : EverySet NCDDiagnosis
    , medicalConditions : EverySet MedicalCondition
    , coMorbidities : EverySet MedicalCondition
    , hivTestResult : Maybe TestResult
    , hivTestExecutionNote : Maybe TestExecutionNote
    }


type alias PMTCTDataItem =
    { identifier : PersonIdentifier
    , startDate : NominalDate
    , endDate : NominalDate
    }


type alias SPVDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , birthDate : NominalDate
    , gender : Gender
    , encounters : List SPVEncounterDataItem
    }


type alias SPVEncounterDataItem =
    { startDate : NominalDate
    , encounterType : WellChildEncounterType
    , warnings : EverySet EncounterWarning
    , zscoreStunting : Maybe Float
    , zscoreUnderweight : Maybe Float
    , zscoreWasting : Maybe Float
    , muac : Maybe Float
    , nutritionSigns : EverySet ChildNutritionSign
    , bcgImminizationDates : EverySet NominalDate
    , opvImminizationDates : EverySet NominalDate
    , dtpImminizationDates : EverySet NominalDate
    , dtpStandaloneImminizationDates : EverySet NominalDate
    , pcv13ImminizationDates : EverySet NominalDate
    , rotarixImminizationDates : EverySet NominalDate
    , ipvImminizationDates : EverySet NominalDate
    , mrImminizationDates : EverySet NominalDate
    , hpvImminizationDates : EverySet NominalDate
    }


type alias ChildScoreboardDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , birthDate : NominalDate
    , gender : Gender
    , encounters : List ChildScoreboardEncounterDataItem
    }


type alias ChildScoreboardEncounterDataItem =
    { startDate : NominalDate
    , bcgImminizationDates : EverySet NominalDate
    , opvImminizationDates : EverySet NominalDate
    , dtpImminizationDates : EverySet NominalDate
    , dtpStandaloneImminizationDates : EverySet NominalDate
    , pcv13ImminizationDates : EverySet NominalDate
    , rotarixImminizationDates : EverySet NominalDate
    , ipvImminizationDates : EverySet NominalDate
    , mrImminizationDates : EverySet NominalDate
    }


type alias NutritionIndividualDataItem =
    { identifier : PersonIdentifier
    , created : NominalDate
    , birthDate : NominalDate
    , encounters : List NutritionIndividualEncounterDataItem
    }


type alias NutritionIndividualEncounterDataItem =
    { startDate : NominalDate
    , encounterType : NutritionEncounterType
    , zscoreStunting : Maybe Float
    , zscoreUnderweight : Maybe Float
    , zscoreWasting : Maybe Float
    , muac : Maybe Float
    , nutritionSigns : EverySet ChildNutritionSign
    }


type alias NutritionGroupDataItem =
    { identifier : PersonIdentifier
    , encounters : List NutritionGroupEncounterDataItem
    }


type alias NutritionGroupEncounterDataItem =
    { startDate : NominalDate
    , zscoreStunting : Maybe Float
    , zscoreUnderweight : Maybe Float
    , zscoreWasting : Maybe Float
    , muac : Maybe Float
    , nutritionSigns : EverySet ChildNutritionSign
    }


type alias NutritionDataItem =
    { identifier : PersonIdentifier
    , encounters : List NutritionEncounterDataItem
    }


type alias NutritionEncounterDataItem =
    { startDate : NominalDate
    , warnings : EverySet EncounterWarning
    , zscoreStunting : Maybe Float
    , zscoreUnderweight : Maybe Float
    , zscoreWasting : Maybe Float
    , muac : Maybe Float
    , nutritionSigns : EverySet ChildNutritionSign
    }
