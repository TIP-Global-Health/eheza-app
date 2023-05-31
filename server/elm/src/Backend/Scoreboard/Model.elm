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
    { row1 : VaccinationProgressDict
    , row2 : List NominalDate
    , row3 : List NominalDate
    , row4 : List NominalDate
    , row5 : UniversalInterventionECDData
    }


emptyUniversalInterventionData : UniversalInterventionData
emptyUniversalInterventionData =
    UniversalInterventionData Dict.empty [] [] [] emptyUniversalInterventionECDData


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


type alias ECDEncounterData =
    { date : NominalDate
    , warning : ECDWarning
    , signs : List ECDSign
    }


type ECDWarning
    = WarningECDMilestoneBehind
    | WarningECDMilestoneReferToSpecialist
    | NoECDMilstoneWarning


type ECDSign
    = -- From 5 weeks.
      FollowMothersEyes
    | MoveArmsAndLegs
      -- From 13 weeks.
    | RaiseHandsUp
    | Smile
    | RollSideways
      -- From 6 months (minors).
    | BringHandsToMouth
    | HoldHeadWithoutSupport
    | HoldAndShakeToys
    | ReactToSuddenSounds
    | UseConsonantSounds
      -- From 6 months (majors).
    | RespondToSoundWithSound
    | TurnHeadWhenCalled
    | SitWithoutSupport
    | SmileBack
    | RollTummyToBack
    | ReachForToys
      -- From 15 months.
    | UseSimpleGestures
    | StandOnTheirOwn
    | CopyDuringPlay
    | SayMamaDada
    | CanHoldSmallObjects
      -- From 18 months.
    | LooksWhenPointedAt
    | UseSingleWords
    | WalkWithoutHelp
    | PlayPretend
    | PointToThingsOfInterest
      -- From 2 years.
    | UseShortPhrases
    | InterestedInOtherChildren
    | FollowSimpleInstructions
    | KickBall
    | PointAtNamedObjects
      -- From 3 years.
    | DressThemselves
    | WashHandsGoToToiled
    | KnowsColorsAndNumbers
    | UseMediumPhrases
    | PlayMakeBelieve
      -- From 4 years.
    | FollowThreeStepInstructions
    | StandOnOneFootFiveSeconds
    | UseLongPhrases
    | ShareWithOtherChildren
    | CountToTen
    | NoECDSigns


type alias UniversalInterventionECDData =
    { encountersData : List ECDEncounterData
    , ecdMilestonesStatusByMonth : List ECDStatus
    }


emptyUniversalInterventionECDData : UniversalInterventionECDData
emptyUniversalInterventionECDData =
    UniversalInterventionECDData [] []


type PediatricCareMilestone
    = Milestone6Weeks
    | Milestone14Weeks
    | Milestone6Months
    | Milestone9Months
    | Milestone12Months
    | Milestone15Months
    | Milestone18Months
    | Milestone2Years
    | Milestone3Years
    | Milestone4Years


type ECDStatus
    = StatusOnTrack
    | StatusECDBehind
    | StatusOffTrack
    | NoECDStatus


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
