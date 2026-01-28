module Backend.Measurement.Model exposing (..)

{-| This module represents various measurements to be stored on the backend,
and cached in local storage.
-}

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (WebData)
import Translate.Model exposing (Language)



-- GENERAL CASE


{-| A base that expresses some things all the measurements
have in common, plus two things whose type varies:

  - the type of the ID for the encounter this measurement
    takes place in
  - the type of the value for this measurement

-}
type alias Measurement encounter value =
    { dateMeasured : NominalDate
    , nurse : Maybe NurseId
    , healthCenter : Maybe HealthCenterId
    , participantId : PersonId
    , deleted : Bool
    , encounterId : Maybe encounter
    , value : value
    }


type alias GroupMeasurement value =
    Measurement SessionId value


type alias PrenatalMeasurement value =
    Measurement PrenatalEncounterId value


type alias NutritionMeasurement value =
    Measurement NutritionEncounterId value


type alias AcuteIllnessMeasurement value =
    Measurement AcuteIllnessEncounterId value


type alias HomeVisitMeasurement value =
    Measurement HomeVisitEncounterId value


type alias WellChildMeasurement value =
    Measurement WellChildEncounterId value


type alias NCDMeasurement value =
    Measurement NCDEncounterId value


type alias ChildScoreboardMeasurement value =
    Measurement ChildScoreboardEncounterId value


type alias TuberculosisMeasurement value =
    Measurement TuberculosisEncounterId value


type alias HIVMeasurement value =
    Measurement HIVEncounterId value



-- GROUP MEASUREMENT TYPES


{-| The string represents the URL of the photo -- that is, the URL which
we can reference in order to display the photo.
-}
type ImageUrl
    = ImageUrl String


type alias Photo =
    GroupMeasurement ImageUrl


{-| For the various measurements that are floats, we wrap them in a type to
distinguish them, and make the units explicit.
-}
type MuacInCm
    = MuacInCm Float


type alias Muac =
    GroupMeasurement MuacInCm


type ColorAlertIndication
    = ColorAlertGreen
    | ColorAlertRed
    | ColorAlertYellow


type HeightInCm
    = HeightInCm Float


type alias Height =
    GroupMeasurement HeightInCm


type WeightInKg
    = WeightInKg Float


type WeightInGrm
    = WeightInGrm Float


type alias Weight =
    GroupMeasurement WeightInKg


type FamilyPlanningSign
    = AutoObservation
    | Condoms
    | CycleBeads
    | CycleCounting
    | Hysterectomy
    | Implants
    | Injectables
    | IUD
    | LactationAmenorrhea
    | NoFamilyPlanning
    | OralContraceptives
    | Spermicide
    | TubalLigatures
    | Vasectomy


type alias FamilyPlanning =
    GroupMeasurement (EverySet FamilyPlanningSign)


type LactationSign
    = Breastfeeding
    | NoLactationSigns


type alias Lactation =
    GroupMeasurement (EverySet LactationSign)


type alias LactationForm =
    { breastfeeding : Maybe Bool
    }


type DistributionNotice
    = DistributedFully
    | DistributedPartiallyLackOfStock
    | DistributedPartiallyOther


type alias Fbf =
    GroupMeasurement FbfValue


type alias FbfValue =
    { distributedAmount : Float
    , distributionNotice : DistributionNotice
    }


type alias ParticipantConsent =
    GroupMeasurement ParticipantConsentValue


type alias ParticipantConsentValue =
    { language : Language
    , formId : ParticipantFormId
    }


type alias Attendance =
    GroupMeasurement Bool


type ChildNutritionSign
    = AbdominalDistension
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | PoorAppetite
    | NormalChildNutrition


type alias ChildNutrition =
    GroupMeasurement NutritionValue


type alias NutritionValue =
    { signs : EverySet ChildNutritionSign
    , assesment : EverySet NutritionAssessment
    }


emptyNutritionValue : NutritionValue
emptyNutritionValue =
    { signs = EverySet.empty
    , assesment = EverySet.empty
    }


type alias CounselingSession =
    GroupMeasurement ( CounselingTiming, EverySet CounselingTopicId )


type alias GroupSendToHC =
    GroupMeasurement SendToHCValue


type alias GroupHealthEducation =
    GroupMeasurement HealthEducationValue


type alias ContributingFactors =
    GroupMeasurement (EverySet ContributingFactorsSign)


type alias FollowUp =
    GroupMeasurement NutritionFollowUpValue


type alias NutritionFollowUpValue =
    { options : EverySet FollowUpOption
    , resolutionDate : Maybe NominalDate
    , assesment : EverySet NutritionAssessment
    }


type NutritionAssessment
    = AssesmentAcuteMalnutritionModerate
    | AssesmentAcuteMalnutritionSevere
    | AssesmentUnderweightModerate
    | AssesmentUnderweightSevere
    | AssesmentDangerSignsNotPresent
    | AssesmentDangerSignsPresent
    | AssesmentMalnutritionSigns (List ChildNutritionSign)
    | AssesmentConsecutiveWeightLoss
    | NoNutritionAssessment


type ContributingFactorsSign
    = FactorLackOfBreastMilk
    | FactorMaternalMastitis
    | FactorPoorSuck
    | FactorDiarrheaOrVomiting
    | NoContributingFactorsSign


type FollowUpOption
    = OneDay
    | ThreeDays
    | OneWeek
    | TwoWeeks
    | OneMonth
    | TwoMonths
    | ThreeMonths
    | FollowUpNotNeeded


type alias GroupNCDA =
    GroupMeasurement NCDAValue


type alias NCDAValue =
    { signs : EverySet NCDASign
    , birthWeight : Maybe WeightInGrm
    , ancVisitsDates : EverySet NominalDate
    , receivesVitaminA : Maybe ReceiveOption
    , stuntingLevel : Maybe StuntingLevel
    , weight : Maybe WeightInKg
    , muac : Maybe MuacInCm
    }


type NCDASign
    = AppropriateComplementaryFeeding
    | BeneficiaryCashTransfer
    | BornWithBirthDefect
    | BreastfedForSixMonths
    | ChildBehindOnVaccination
    | ChildGotDiarrhea
    | ChildReceivesFBF
    | ChildTakingFBF
    | -- This option is not an actual sign, as we got dedicated field for it,
      -- to support 'not applicable' value. We keep it though, to maintain
      -- form display logic which is common for all signs.
      ChildReceivesVitaminA
    | ChildReceivesDewormer
    | ChildReceivesECD
    | ChildWithDisability
    | ConditionalFoodItems
    | FiveFoodGroups
    | HasCleanWater
    | HasHandwashingFacility
    | HasKitchenGarden
    | HasToilets
    | InsecticideTreatedBednets
    | MealsAtRecommendedTimes
    | OngeraMNP
    | ReceivingCashTransfer
    | ReceivingSupport
    | SupplementsDuringPregnancy
    | TakenSupplementsPerGuidance
    | TakingOngeraMNP
    | TreatedForAcuteMalnutrition
    | ShowsEdemaSigns
    | NoNCDASigns


type ReceiveOption
    = OptionReceive
    | OptionNotReceive
    | OptionNotApplicable


type StuntingLevel
    = LevelGreen
    | LevelYellow
    | LevelRed



-- NUTRITION MEASUREMENTS


type alias NutritionMuac =
    NutritionMeasurement MuacInCm


type alias NutritionHeight =
    NutritionMeasurement HeightInCm


type alias NutritionNutrition =
    NutritionMeasurement NutritionValue


type alias NutritionPhoto =
    NutritionMeasurement ImageUrl


type alias NutritionWeight =
    NutritionMeasurement WeightInKg


type alias NutritionSendToHC =
    NutritionMeasurement SendToHCValue


type alias NutritionHealthEducation =
    NutritionMeasurement HealthEducationValue


type alias NutritionContributingFactors =
    NutritionMeasurement (EverySet ContributingFactorsSign)


type alias NutritionFollowUp =
    NutritionMeasurement NutritionFollowUpValue



-- HOME VISIT MEASUREMENTS


type alias NutritionFeeding =
    HomeVisitMeasurement NutritionFeedingValue


type alias NutritionFeedingValue =
    { signs : EverySet NutritionFeedingSign
    , supplementType : NutritionSupplementType
    , sachetsPerDay : Float
    }


type NutritionFeedingSign
    = ReceiveSupplement
    | RationPresentAtHome
    | EnoughTillNextSession
    | SupplementShared
    | EncouragedToEat
    | RefusingToEat
    | FeedingSignBreastfeeding
    | CleanWaterAvailable
    | EatenWithWater
    | NoNutritionFeedingSigns


type NutritionSupplementType
    = FortifiedPorridge
    | Rutf
    | Ongera
    | TherapeuticMilk
    | NoNutritionSupplementType


type alias NutritionHygiene =
    HomeVisitMeasurement NutritionHygieneValue


type alias NutritionHygieneValue =
    { signs : EverySet NutritionHygieneSign
    , mainWaterSource : MainWaterSource
    , waterPreparationOption : WaterPreparationOption
    }


type NutritionHygieneSign
    = SoapInTheHouse
    | WashHandsBeforeFeeding
    | FoodCovered
    | NoNutritionHygieneSigns


type MainWaterSource
    = PipedWaterToHome
    | PublicWaterTap
    | RainWaterCollectionSystem
    | NaturalSourceFlowingWater
    | NaturalSourceStandingWater
    | BottledWater


type WaterPreparationOption
    = Boiled
    | PurificationSolution
    | Filtered
    | Bottled
    | NoWaterPreparationOption


type alias NutritionFoodSecurity =
    HomeVisitMeasurement NutritionFoodSecurityValue


type alias NutritionFoodSecurityValue =
    { signs : EverySet NutritionFoodSecuritySign
    , mainIncomeSource : MainIncomeSource
    }


type NutritionFoodSecuritySign
    = HouseholdGotFood
    | NoNutritionFoodSecuritySigns


type MainIncomeSource
    = HomeBasedAgriculture
    | CommercialAgriculture
    | PublicEmployee
    | PrivateBusinessEmpployee


type alias NutritionCaringValue =
    { signs : EverySet NutritionCaringSign
    , caringOption : CaringOption
    }


type alias NutritionCaring =
    HomeVisitMeasurement NutritionCaringValue


type NutritionCaringSign
    = ParentsAliveHealthy
    | ChildClean
    | NoCaringSigns


type CaringOption
    = CaredByParent
    | CaredByGrandparent
    | CaredBySibling
    | CaredByNeighbor
    | CaredByHouseHelper
    | CaredByDaycare


type alias NutritionNCDA =
    NutritionMeasurement NCDAValue



-- PRENATAL MEASUREMENTS


type alias BreastExamValue =
    { exam : EverySet BreastExamSign
    , dischargeType : Maybe DischargeType
    , selfGuidance : Bool
    }


type BreastExamSign
    = Mass
    | Discharge
    | Infection
    | Warmth
    | NormalBreast


type DischargeType
    = DischargeMilky
    | DischargeClear
    | DischargeBrownOrBloody
    | DischargeYellow
    | DischargeGreen


type alias BreastExam =
    PrenatalMeasurement BreastExamValue


type alias CorePhysicalExam =
    PrenatalMeasurement CorePhysicalExamValue


type alias CorePhysicalExamValue =
    { hairHead : EverySet HairHeadCPESign
    , eyes : EverySet EyesCPESign
    , heart : EverySet HeartCPESign
    , heartMurmur : Bool
    , neck : EverySet NeckCPESign
    , lungs : EverySet LungsCPESign
    , abdomen : EverySet AbdomenCPESign
    , hands : EverySet HandsCPESign
    , legs : EverySet LegsCPESign
    }


type HairHeadCPESign
    = BrittleHairCPE
    | NormalHairHead


type EyesCPESign
    = PaleConjuctiva
    | NormalEyes


type HeartCPESign
    = IrregularRhythm
    | NormalRateAndRhythm
    | SinusTachycardia


type NeckCPESign
    = EnlargedThyroid
    | EnlargedLymphNodes
    | NormalNeck


type LungsCPESign
    = Wheezes
    | Crackles
    | NormalLungs


type AbdomenCPESign
    = Hepatomegaly
    | Splenomegaly
    | TPRightUpper
    | TPRightLower
    | TPLeftUpper
    | TPLeftLower
    | Hernia
    | NormalAbdomen


type HandsCPESign
    = PallorHands
    | EdemaHands
    | NormalHands


type LegsCPESign
    = PallorLegs
    | EdemaLegs
    | NormalLegs


type DangerSign
    = VaginalBleeding
    | HeadacheBlurredVision
    | Convulsions
    | AbdominalPain
    | DifficultyBreathing
    | Fever
    | ExtremeWeakness
    | ImminentDelivery
    | Labor
    | LooksVeryIll
    | SevereVomiting
    | Unconscious
    | GushLeakingVaginalFluid
    | PrematureOnsetContractions
    | NoDangerSign


type alias DangerSigns =
    PrenatalMeasurement DangerSignsValue


type alias DangerSignsValue =
    { signs : EverySet DangerSign
    , postpartumMother : EverySet PostpartumMotherDangerSign
    , postpartumChild : EverySet PostpartumChildDangerSign
    }


type PostpartumMotherDangerSign
    = PostpartumMotheUterineBleeding
    | PostpartumMotherFever
    | PostpartumMotherMigraine
    | PostpartumMotherParalysis
    | PostpartumMotherAcuteAbdominalPain
    | PostpartumMotherLabouredBreathing
    | NoPostpartumMotherDangerSigns


type PostpartumChildDangerSign
    = PostpartumChildInabilityToSuckle
    | PostpartumChildParalysis
    | PostpartumChildLabouredBreathing
    | PostpartumChildAbnormalTemperature
    | PostpartumChildInactiveNoMovement
    | PostpartumChildBodyTurnedYellow
    | NoPostpartumChildDangerSigns


type alias LastMenstrualPeriodValue =
    { date : NominalDate
    , -- Depricated, but kept for backward compatibility.
      prePregnancyWeight : Maybe WeightInKg
    , confident : Bool
    , notConfidentReason : Maybe LmpDateNotConfidentReason
    , lateFirstVisitReason : Maybe LateFirstANCVisitReason
    , confirmation : Bool
    }


type LmpDateNotConfidentReason
    = ReasonIrregularMenses
    | ReasonOnFamilyPlanningMethod
    | ReasonCanNotRememberDates


type LateFirstANCVisitReason
    = ReasonLackOfFunds
    | ReasonLackOfHealthInsurance
    | ReasonPartnerAccompanimentRequirement
    | ReasonUndetectedPregnancy
    | ReasonLongDistancesToHealthFacilities
    | ReasonNegativePastExperiences
    | ReasonTraditionalBeliefs
    | ReasonLackOfAwarenessToANC
    | ReasonDelayedRecognitionOfSymptoms
    | ReasonOtherReasons


type alias LastMenstrualPeriod =
    PrenatalMeasurement LastMenstrualPeriodValue


type MedicalHistorySign
    = Asthma
    | AutoimmuneDisease
    | CardiacDisease
    | Diabetes
    | HypertensionBeforePregnancy
    | RenalDisease
    | NoMedicalHistorySigns
      -- Sign is kept as legacy value, to preserve data recorded during encounters,
      -- before it was split into 2 signs at MedicalHistoryPhysicalCondition.
    | UterineMyoma
      -- Sign is kept as legacy value, to preserve data recorded during encounters,
      -- before it was replaced by MedicalHistoryMentalHealthIssue signs.
    | MentalHealthHistory
      -- @todo: Below signs are deprecated. Can be removed around January 2025.
    | BowedLegs
    | HIV
    | TuberculosisPast
    | TuberculosisPresent


type MedicalHistoryPhysicalCondition
    = PhysicalConditionUterineMyomaCurrent
    | PhysicalConditionUterineMyomaSurgicalResection
    | PhysicalConditionBowedLegs
    | NoMedicalHistoryPhysicalCondition
    | MigrateMedicalHistoryPhysicalCondition


type MedicalHistoryInfectiousDisease
    = InfectiousDiseasesHIV
    | InfectiousDiseasesTuberculosisPast
    | InfectiousDiseasesTuberculosisPresent
    | NoMedicalHistoryInfectiousDisease


type MedicalHistoryMentalHealthIssue
    = MentalHealthIssueGeneralDepression
    | MentalHealthIssuePerinatalDepression
    | MentalHealthIssueSchizophrenia
    | MentalHealthIssueTrauma
    | NoMedicalHistoryMentalHealthIssue


type alias MedicalHistory =
    PrenatalMeasurement MedicalHistoryValue


type alias MedicalHistoryValue =
    { signs : EverySet MedicalHistorySign
    , physicalConditions : EverySet MedicalHistoryPhysicalCondition
    , infectiousDiseases : EverySet MedicalHistoryInfectiousDisease
    , mentalHealthIssues : EverySet MedicalHistoryMentalHealthIssue
    , preeclampsiaInFamily : OccursInFamilySign
    }


type OccursInFamilySign
    = DoesOccur
    | DoesNotOccur
    | NotKnownIfOccurs


type MedicationSign
    = IronAndFolicAcidSupplement
      -- This option is deprecated. However, we keep it, to support
      -- ongoing pregnancies where it was used already.
      -- Considering deployment schedule, it will be
      -- safe to remove starting Jan 2023.
    | DewormingPill
    | Mebendazole
    | PostpartumFolicAcid
    | PostpartumVitaminA
    | NoMedication


type MedicationTreatmentSign
    = MedicationTreatmentStillTaking
    | MedicationTreatmentMissedDoses
    | MedicationTreatmentAdverseEvents
    | MedicationTreatmentAdverseEventsHospitalization
    | NoMedicationTreatment


type HIVTreatmentSign
    = HIVTreatmentStillTaking
    | HIVTreatmentMissedDoses
    | HIVTreatmentAdverseEvents
    | HIVTreatmentAdverseEventsHospitalization
    | HIVTreatmentMedicineByPMTCT
    | HIVTreatmentNoMedicineNotSeenAtPMTCT
    | HIVTreatmentNoMedicineOutOfStock
    | HIVTreatmentNoMedicinePatientRefused
    | HIVTreatmentNoMedicineOther
    | NoHIVTreatment


type alias Medication =
    PrenatalMeasurement MedicationValue


type alias MedicationValue =
    { signs : Maybe (EverySet MedicationSign)
    , hivTreatment : Maybe (EverySet HIVTreatmentSign)
    , hypertensionTreatment : Maybe (EverySet MedicationTreatmentSign)
    , malariaTreatment : Maybe (EverySet MedicationTreatmentSign)
    , anemiaTreatment : Maybe (EverySet MedicationTreatmentSign)
    , syphilisTreatment : Maybe (EverySet MedicationTreatmentSign)
    }


type alias ObstetricalExamValue =
    { fundalPalpable : Bool
    , fundalHeight : Maybe HeightInCm
    , fetalPresentation : FetalPresentation
    , fetalMovement : Bool
    , fetalHeartRate : Int
    , cSectionScar : CSectionScar
    }


type alias ObstetricalExam =
    PrenatalMeasurement ObstetricalExamValue


type FetalPresentation
    = Transverse
    | FetalBreech
    | Cephalic
    | Twins
    | UnclearImprecise
    | Unknown


type CSectionScar
    = Vertical
    | Horizontal
    | NoScar


type alias ObstetricHistoryValue =
    { currentlyPregnant : Bool
    , termPregnancy : Int
    , preTermPregnancy : Int
    , stillbirthsAtTerm : Int
    , stillbirthsPreTerm : Int
    , abortions : Int
    , liveChildren : Int
    }


type alias ObstetricHistory =
    PrenatalMeasurement ObstetricHistoryValue


type alias ObstetricHistoryStep2Value =
    { cSections : Int
    , cSectionReason : Maybe (EverySet CSectionReason)
    , previousDelivery : EverySet PreviousDeliverySign
    , previousDeliveryPeriod : EverySet PreviousDeliveryPeriod

    -- @todo: obstetricHistory is depricated, and can be removed
    -- around January 2025.
    , obstetricHistory : EverySet ObstetricHistorySign
    , signs : EverySet ObstetricHistoryStep2Sign
    }


type CSectionReason
    = Breech
    | Emergency
    | FailureToProgress
    | None
    | Other
    | PreviousCSection


type PreviousDeliveryPeriod
    = LessThan18Month
    | MoreThan5Years
    | MoreThan10Years
    | Neither


type PreviousDeliverySign
    = CSectionInPast
    | CSectionInPreviousDelivery
    | NoPreviousDeliverySign
      -- @todo: Below signs are deprecated. Can be removed around January 2025.
    | StillbornPreviousDelivery
    | BabyDiedOnDayOfBirthPreviousDelivery
    | PartialPlacentaPreviousDelivery
    | SevereHemorrhagingPreviousDelivery
    | ConvulsionsPreviousDelivery
    | ConvulsionsAndUnconsciousPreviousDelivery


{-| @todo: ObstetricHistorySign is deprecated. Can be removed around January 2025.
-}
type ObstetricHistorySign
    = SuccessiveAbortions
    | SuccessivePrematureDeliveries
    | PreeclampsiaPreviousPregnancy
    | GestationalDiabetesPreviousPregnancy
    | IncompleteCervixPreviousPregnancy
    | RhNegative
    | NoObstetricHistorySign


type ObstetricHistoryStep2Sign
    = ObstetricHistoryPreeclampsiaPreviousPregnancy
    | ObstetricHistoryGestationalDiabetesPreviousPregnancy
    | ObstetricHistoryIncompleteCervixPreviousPregnancy
    | ObstetricHistoryBabyDiedOnDayOfBirthPreviousDelivery
    | ObstetricHistoryPartialPlacentaPreviousDelivery
    | ObstetricHistoryPlacentaAbruptionPreviousDelivery
    | ObstetricHistorySevereHemorrhagingPreviousDelivery
    | ObstetricHistoryConvulsionsPreviousDelivery
    | ObstetricHistoryConvulsionsAndUnconsciousPreviousDelivery
    | ObstetricHistoryChildWithLowBirthweightPreviousDelivery
    | ObstetricHistorySmallForGestationalAgePreviousDelivery
    | ObstetricHistoryIntraUterineDeathPreviousDelivery
    | NoObstetricHistoryStep2Sign
    | MigrateObstetricHistoryStep2Sign


type alias ObstetricHistoryStep2 =
    PrenatalMeasurement ObstetricHistoryStep2Value


type alias PrenatalFamilyPlanning =
    PrenatalMeasurement (EverySet FamilyPlanningSign)


type alias PrenatalNutritionValue =
    { height : HeightInCm
    , weight : WeightInKg
    , muac : MuacInCm
    }


type alias PrenatalNutrition =
    PrenatalMeasurement PrenatalNutritionValue


type alias PrenatalPhoto =
    PrenatalMeasurement ImageUrl


type alias MalariaPrevention =
    PrenatalMeasurement MalariaPreventionValue


type alias MalariaPreventionValue =
    { resources : EverySet MalariaPreventionSign
    , phaseRecorded : PhaseRecorded
    }


type MalariaPreventionSign
    = MosquitoNet
    | NoMalariaPreventionSigns


type PhaseRecorded
    = PhaseInitial
    | PhaseRecurrent


type SocialHistorySign
    = AccompaniedByPartner
    | PartnerHivCounseling
    | NoSocialHistorySign


type alias SocialHistoryValue =
    EverySet SocialHistorySign


type alias SocialHistory =
    PrenatalMeasurement SocialHistoryValue


type alias VitalsValue =
    { sys : Maybe Float
    , dia : Maybe Float
    , heartRate : Maybe Int
    , respiratoryRate : Int
    , bodyTemperature : Float
    , sysRepeated : Maybe Float
    , diaRepeated : Maybe Float
    }


type alias Vitals =
    PrenatalMeasurement VitalsValue


type alias BirthPlanValue =
    { signs : EverySet BirthPlanSign
    , familyPlanning : EverySet FamilyPlanningSign
    }


type alias BirthPlan =
    PrenatalMeasurement BirthPlanValue


type BirthPlanSign
    = Insurance
    | BoughtClothes
    | CaregiverAccompany
    | SavedMoney
    | Transportation
    | NoBirthPlan


type alias PregnancyTest =
    PrenatalMeasurement PregnancyTestResult


type PregnancyTestResult
    = PregnancyTestPositive
    | PregnancyTestNegative
    | PregnancyTestIndeterminate
    | PregnancyTestUnableToConduct


type alias PrenatalHealthEducation =
    PrenatalMeasurement PrenatalHealthEducationValue


type alias PrenatalHealthEducationValue =
    { signs : EverySet PrenatalHealthEducationSign
    , signsPhase2 : Maybe (EverySet PrenatalHealthEducationSign)
    }


type PrenatalHealthEducationSign
    = EducationExpectations
    | EducationVisitsReview
    | EducationWarningSigns
    | EducationHemorrhaging
    | EducationFamilyPlanning
    | EducationBreastfeeding
    | EducationImmunization
    | EducationHygiene
    | EducationPositiveHIV
    | EducationSaferSexHIV
    | EducationPartnerTesting
    | EducationNauseaVomiting
    | EducationLegCramps
    | EducationLowBackPain
    | EducationConstipation
    | EducationHeartburn
    | EducationVaricoseVeins
    | EducationLegPainRedness
    | EducationPelvicPain
    | EducationSaferSex
    | EducationHIVDetectableViralLoad
    | EducationMentalHealth
    | EducationDiabetes
    | EducationEarlyMastitisOrEngorgment
    | EducationMastitis
    | EducationGrief
    | EducationHIVPartnerPresence
    | NoPrenatalHealthEducationSigns


type alias PrenatalFollowUp =
    PrenatalMeasurement PrenatalFollowUpValue


type alias PrenatalFollowUpValue =
    { options : EverySet FollowUpOption
    , resolutionDate : Maybe NominalDate
    , assesment : PrenatalAssesment
    }


type PrenatalAssesment
    = AssesmentNormalPregnancy
    | AssesmentHighRiskPregnancy


type alias PrenatalSendToHC =
    PrenatalMeasurement PrenatalReferralValue


type alias PrenatalReferralValue =
    { sendToHCSigns : Maybe (EverySet SendToHCSign)
    , reasonForNotSendingToHC : Maybe ReasonForNonReferral
    , referToFacilitySigns : Maybe (EverySet ReferToFacilitySign)
    , facilityNonReferralReasons : Maybe (EverySet NonReferralSign)
    }


type ReferToFacilitySign
    = ReferToHospital
    | ReferralFormHospital
    | ReferToMentalHealthSpecialist
    | ReferralFormMentalHealthSpecialist
    | AccompanyToMentalHealthSpecialist
    | ReferToARVProgram
    | ReferralFormARVProgram
    | AccompanyToARVProgram
    | ReferToNCDProgram
    | ReferralFormNCDProgram
    | AccompanyToNCDProgram
    | ReferToANCServices
    | ReferralFormANCServices
    | AccompanyToANCServices
    | ReferToUltrasound
    | ReferralFormUltrasound
    | NoReferToFacilitySigns


type NonReferralSign
    = NonReferralReasonHospital ReasonForNonReferral
    | NonReferralReasonMentalHealthSpecialist ReasonForNonReferral
    | NonReferralReasonARVProgram ReasonForNonReferral
    | NonReferralReasonNCDProgram ReasonForNonReferral
    | NonReferralReasonANCServices ReasonForNonReferral
    | NonReferralReasonUltrasound ReasonForNonReferral
    | NoNonReferralSigns


type ReferralFacility
    = FacilityHealthCenter
    | FacilityHospital
    | FacilityMentalHealthSpecialist
    | FacilityARVProgram
    | FacilityNCDProgram
    | FacilityANCServices
    | FacilityUltrasound


type alias PrenatalAppointmentConfirmationValue =
    { date : NominalDate
    }


type alias PrenatalAppointmentConfirmation =
    PrenatalMeasurement PrenatalAppointmentConfirmationValue


type alias PrenatalMalariaTest =
    PrenatalMeasurement MalariaTestValue


type alias MalariaTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , bloodSmearResult : BloodSmearResult
    }


type TestExecutionNote
    = TestNoteRunToday
    | TestNoteRunPreviously
    | TestNoteLackOfReagents
    | TestNoteLackOfOtherSupplies
    | TestNoteNoEquipment
    | TestNoteBrokenEquipment
    | TestNoteNotIndicated
    | TestNoteKnownAsPositive
    | TestNoteToBeDoneAtHospital
    | TestNoteRunConfirmedByLabTech
    | TestNoteNotPresent


type TestResult
    = TestPositive
    | TestNegative
    | TestIndeterminate


type BloodSmearResult
    = BloodSmearNegative
    | BloodSmearPlus
    | BloodSmearPlusPlus
    | BloodSmearPlusPlusPlus
    | BloodSmearNotTaken
      -- Set on initial phase, when Malaria test is not
      -- taken, and blood smear is ordered at lab.
    | BloodSmearPendingInput


type alias PrenatalHIVTest =
    PrenatalMeasurement HIVTestValue


type alias HIVTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , hivSigns : Maybe (EverySet PrenatalHIVSign)
    }


type PrenatalHIVSign
    = HIVProgramHC
    | PartnerHIVPositive
    | PartnerTakingARV
    | PartnerSurpressedViralLoad
      -- This option is an indicator.
      -- When Lab tech fills the result, they do not
      -- answer follow up question. We use this optin to indicate
      -- that nurse will have to complete the results follow up.
    | PrenatalHIVSignPendingInput
    | NoPrenatalHIVSign


type alias PrenatalHIVPCRTest =
    PrenatalMeasurement HIVPCRTestValue


type HIVPCRResult
    = ResultSuppressedViralLoad
    | ResultDetectibleViralLoad Float


type alias HIVPCRTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , hivViralLoadStatus : Maybe ViralLoadStatus
    , hivViralLoad : Maybe Float
    }


type ViralLoadStatus
    = ViralLoadDetectable
    | ViralLoadUndetectable


type alias PrenatalHepatitisBTest =
    PrenatalMeasurement (HepatitisBTestValue PrenatalEncounterId)


type alias HepatitisBTestValue encounterId =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , originatingEncounter : Maybe encounterId
    }


type alias PrenatalSyphilisTest =
    PrenatalMeasurement (SyphilisTestValue PrenatalEncounterId)


type alias SyphilisTestValue encounterId =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , symptoms : Maybe (EverySet IllnessSymptom)
    , originatingEncounter : Maybe encounterId
    }


type IllnessSymptom
    = IllnessSymptomHeadache
    | IllnessSymptomVisionChanges
    | IllnessSymptomRash
    | IllnessSymptomPainlessUlcerMouth
    | IllnessSymptomPainlessUlcerGenitals
      -- This option is an indicator.
      -- When Lab tech fills the result, they do not
      -- answer follow up quesrtion. We use this optin to indicate
      -- that nurse will have to complete the results follow up.
    | IllnessSymptomPendingInput
    | NoIllnessSymptoms


type alias PrenatalHemoglobinTest =
    PrenatalMeasurement HemoglobinTestValue


type alias HemoglobinTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , hemoglobinCount : Maybe Float
    }


type alias PrenatalRandomBloodSugarTest =
    PrenatalMeasurement (RandomBloodSugarTestValue PrenatalEncounterId)


type alias RandomBloodSugarTestValue encounterId =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , sugarCount : Maybe Float
    , originatingEncounter : Maybe encounterId
    }


type TestPrerequisite
    = PrerequisiteFastFor12h
    | PrerequisiteImmediateResult
    | NoTestPrerequisites


type alias PrenatalBloodGpRsTest =
    PrenatalMeasurement (BloodGpRsTestValue PrenatalEncounterId)


type alias BloodGpRsTestValue encounterId =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , bloodGroup : Maybe BloodGroup
    , rhesus : Maybe Rhesus
    , originatingEncounter : Maybe encounterId
    }


type BloodGroup
    = BloodGroupA
    | BloodGroupB
    | BloodGroupAB
    | BloodGroupO


type Rhesus
    = RhesusPositive
    | RhesusNegative


type alias PrenatalUrineDipstickTest =
    PrenatalMeasurement UrineDipstickTestValue


type alias UrineDipstickTestValue =
    { testVariant : Maybe TestVariant
    , executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , protein : Maybe ProteinValue
    , ph : Maybe PHValue
    , glucose : Maybe GlucoseValue
    , leukocytes : Maybe LeukocytesValue
    , nitrite : Maybe NitriteValue
    , urobilinogen : Maybe UrobilinogenValue
    , haemoglobin : Maybe HaemoglobinValue
    , ketone : Maybe KetoneValue
    , bilirubin : Maybe BilirubinValue
    }


type TestVariant
    = VariantShortTest
    | VariantLongTest


type ProteinValue
    = Protein0
    | ProteinPlus1
    | ProteinPlus2
    | ProteinPlus3
    | ProteinPlus4


type PHValue
    = Ph40
    | Ph45
    | Ph50
    | Ph60
    | Ph65
    | Ph70
    | Ph75
    | Ph80
    | Ph85


type GlucoseValue
    = Glucose0
    | GlucosePlus1
    | GlucosePlus2
    | GlucosePlus3
    | GlucosePlus4


type LeukocytesValue
    = LeukocytesNegative
    | LeukocytesSmall
    | LeukocytesMedium
    | LeukocytesLarge


type NitriteValue
    = NitriteNegative
    | NitritePlus
    | NitritePlusPlus


type UrobilinogenValue
    = Urobilinogen002
    | Urobilinogen10
    | Urobilinogen20
    | Urobilinogen40
    | Urobilinogen80


type HaemoglobinValue
    = HaemoglobinNegative
    | HaemoglobinNonHemolyzedTrace
    | HaemoglobinNonHemolyzedModerate
    | HaemoglobinHemolyzedTrace
    | HaemoglobinSmall
    | HaemoglobinModerate
    | HaemoglobinLarge


type KetoneValue
    = KetoneNegative
    | Ketone5
    | Ketone10
    | Ketone15
    | Ketone40
    | Ketone80
    | Ketone100


type BilirubinValue
    = BilirubinNegative
    | BilirubinSmall
    | BilirubinMedium
    | BilirubinLarge


type alias PrenatalLabsResults =
    PrenatalMeasurement LabsResultsValue


type alias LabsResultsValue =
    { performedTests : EverySet LaboratoryTest
    , completedTests : EverySet LaboratoryTest
    , resolutionDate : NominalDate
    , patientNotified : Bool
    , reviewState : Maybe LabsResultsReviewState
    , testsWithFollowUp : Maybe (EverySet LaboratoryTest)
    }


type LaboratoryTest
    = TestBloodGpRs
    | TestHemoglobin
    | TestHIV
    | TestPartnerHIV
    | TestMalaria
    | TestHIVPCR
    | TestHepatitisB
    | TestRandomBloodSugar
    | TestSyphilis
    | TestUrineDipstick
    | TestVitalsRecheck
    | TestCreatinine
    | TestLiverFunction
    | TestLipidPanel


type LabsResultsReviewState
    = LabsResultsReviewRequested
    | LabsResultsReviewCompleted


type alias PrenatalMedicationDistribution =
    PrenatalMeasurement PrenatalMedicationDistributionValue


type alias PrenatalMedicationDistributionValue =
    { distributionSigns : EverySet MedicationDistributionSign
    , nonAdministrationSigns : EverySet MedicationNonAdministrationSign
    , recommendedTreatmentSigns : Maybe (EverySet RecommendedTreatmentSign)
    , avoidingGuidanceReason : Maybe (EverySet AvoidingGuidanceReason)
    , reinforceTreatmentSigns : Maybe (EverySet ReinforceTreatmentSign)
    }


type RecommendedTreatmentSign
    = -- For Malaria:
      TreatmentQuinineSulphate
    | TreatmentCoartem
    | TreatmentWrittenProtocols
    | TreatmentReferToHospital
    | NoTreatmentForMalaria
      -- For Syphilis:
    | TreatmentPenecilin1
    | TreatmentPenecilin3
    | TreatmentErythromycin
    | TreatmentAzithromycin
    | TreatmentCeftriaxon
    | NoTreatmentForSyphilis
      -- For Hypertension, NCD + Prenatal:
    | TreatmentMethyldopa2
      -- For Hypertension, only Prenatal:
    | TreatmentMethyldopa3
    | TreatmentMethyldopa4
    | TreatmentHypertensionAddCarvedilol
    | TreatmentHypertensionAddAmlodipine
      -- For Hypertension, only NCD:
    | TreatmentHydrochlorothiazide
    | TreatmentAmlodipine
    | TreatmentNifedipine
    | TreatmentCaptopril
    | TreatmentLisinopril
    | TreatmentAtenlol
    | NoTreatmentForHypertension
      -- For Heartburn:
    | TreatmentAluminiumHydroxide
    | TreatmentHealthEducationForHeartburn
      -- For Urinary Tract Infection:
    | TreatmentNitrofurantoin
    | TreatmentAmoxicillin
      -- For Candidiasis:
    | TreatmentClotrimaxazole200
    | TreatmentClotrimaxazole500
      -- For Mastitis:
    | TreatmentCloxacillin
    | TreatmentMastitisAmoxicillin
    | TreatmentPenecilinV
    | TreatmentParacetamol
    | TreatmentIbuprofen
    | NoTreatmentForMastitis
      -- For Diabetes (NCD):
    | TreatmentMetformin1m1e
    | TreatmentGlipenclamide1m1e
    | TreatmentMetformin2m1e
    | TreatmentGlipenclamide2m1e
    | TreatmentMetformin2m2e
    | TreatmentGlipenclamide2m2e
    | TreatmentMetformin2m2eGlipenclamide1m1e
    | TreatmentGlipenclamide2m2eMetformin1m1e
    | NoTreatmentForDiabetes


type AvoidingGuidanceReason
    = AvoidingGuidanceHypertensionLackOfStock
    | AvoidingGuidanceHypertensionKnownAllergy
    | AvoidingGuidanceHypertensionPatientDeclined
    | AvoidingGuidanceHypertensionPatientUnableToAfford
    | AvoidingGuidanceHypertensionReinforceAdherence
    | AvoidingGuidanceHypertensionOther


type ReinforceTreatmentSign
    = ReinforceSignFefol
    | ReinforceSignMMS
    | ReinforceSignRepeatHemoglobinTest
    | NoReinforceTreatmentSigns


type alias PrenatalSymptomReview =
    PrenatalMeasurement PrenatalSymptomReviewValue


type alias PrenatalSymptomReviewValue =
    { symptoms : EverySet PrenatalSymptom
    , symptomQuestions : EverySet PrenatalSymptomQuestion
    , flankPainSign : Maybe PrenatalFlankPainSign
    }


type PrenatalSymptom
    = BurningWithUrination
    | AbnormalVaginalDischarge
    | NauseaAndVomiting
    | Heartburn
    | LegCramps
    | LowBackPain
    | CoughContinuous
    | PelvicPain
    | Constipation
    | VaricoseVeins
    | LegPainRedness
      -- For Postpartum:
    | PostpartumAbdominalPain
    | PostpartumUrinaryIncontinence
    | PostpartumHeadache
    | PostpartumFatigue
    | PostpartumFever
    | PostpartumPerinealPainOrDischarge
    | NoPrenatalSymptoms


type PrenatalSymptomQuestion
    = SymptomQuestionDizziness
    | SymptomQuestionLowUrineOutput
    | SymptomQuestionDarkUrine
    | SymptomQuestionPelvicPainHospitalization
    | SymptomQuestionLegPainRednessLeft
    | SymptomQuestionLegPainful
    | SymptomQuestionLegSwollen
    | SymptomQuestionLegWarm
    | SymptomQuestionNightSweats
    | SymptomQuestionBloodInSputum
    | SymptomQuestionWeightLoss
    | SymptomQuestionSevereFatigue
    | SymptomQuestionVaginalDischarge
    | SymptomQuestionFrequentUrination
    | SymptomQuestionFlankPain
    | SymptomQuestionVaginalItching
    | SymptomQuestionPartnerUrethralDischarge
    | NoSymptomQuestions


type PrenatalFlankPainSign
    = FlankPainLeftSide
    | FlankPainRightSide
    | FlankPainBothSides
    | NoFlankPain


type alias PrenatalOutsideCare =
    PrenatalMeasurement (OutsideCareValue PrenatalDiagnosis)


type alias OutsideCareValue diagnosis =
    { signs : EverySet OutsideCareSign
    , diagnoses : Maybe (EverySet diagnosis)
    , medications : Maybe (EverySet OutsideCareMedication)
    }


type OutsideCareSign
    = SeenAtAnotherFacility
    | GivenNewDiagnoses
    | GivenMedicine
    | PlannedFollowUpCareWithSpecialist
    | NoOutsideCareSigns


type OutsideCareMedication
    = -- For Malaria:
      OutsideCareMedicationQuinineSulphate
    | OutsideCareMedicationCoartem
    | NoOutsideCareMedicationForMalaria
      -- For Syphilis:
    | OutsideCareMedicationPenecilin1
    | OutsideCareMedicationPenecilin3
    | OutsideCareMedicationErythromycin
    | OutsideCareMedicationAzithromycin
    | OutsideCareMedicationCeftriaxon
    | NoOutsideCareMedicationForSyphilis
      -- For Hypertension:
    | OutsideCareMedicationMethyldopa2
    | OutsideCareMedicationMethyldopa3
    | OutsideCareMedicationMethyldopa4
    | OutsideCareMedicationCarvedilol
    | OutsideCareMedicationAmlodipine
    | NoOutsideCareMedicationForHypertension
      -- For HIV:
    | OutsideCareMedicationTDF3TC
    | OutsideCareMedicationDolutegravir
    | NoOutsideCareMedicationForHIV
      -- For Anemia:
    | OutsideCareMedicationIron1
    | OutsideCareMedicationIron2
    | OutsideCareMedicationFolicAcid
    | NoOutsideCareMedicationForAnemia
    | NoOutsideCareMedications


type alias PrenatalMentalHealth =
    PrenatalMeasurement PrenatalMentalHealthValue


type alias PrenatalMentalHealthValue =
    { signs : Dict PrenatalMentalHealthQuestion PrenatalMentalHealthQuestionOption
    , specialistAtHC : Bool
    }


type PrenatalMentalHealthQuestion
    = MentalHealthQuestion1
    | MentalHealthQuestion2
    | MentalHealthQuestion3
    | MentalHealthQuestion4
    | MentalHealthQuestion5
    | MentalHealthQuestion6
    | MentalHealthQuestion7
    | MentalHealthQuestion8
    | MentalHealthQuestion9
    | MentalHealthQuestion10


type PrenatalMentalHealthQuestionOption
    = MentalHealthQuestionOption0
    | MentalHealthQuestionOption1
    | MentalHealthQuestionOption2
    | MentalHealthQuestionOption3


type alias PrenatalTetanusImmunisation =
    PrenatalMeasurement VaccinationValue


type alias PrenatalBreastfeeding =
    PrenatalMeasurement BreastfeedingValue


type alias BreastfeedingValue =
    EverySet BreastfeedingSign


type BreastfeedingSign
    = IsBreastfeeding
    | NotBreastfeedingBreastPain
    | NotBreastfeedingBreastRedness
    | NotBreastfeedingLowMilkProduction
    | NotBreastfeedingProblemsLatching
    | NotBreastfeedingMedicalProblems
    | NotBreastfeedingPersonalChoice
    | NotBreastfeedingOther
    | BreastPain
    | BreastRedness
    | EnoughMilk
    | LatchingWell
    | NoBreastfeedingSigns


type alias PrenatalGUExam =
    PrenatalMeasurement GUExamValue


type alias GUExamValue =
    { vaginalExamSigns : EverySet VaginalExamSign
    , guExamSigns : EverySet GUExamSign
    , postpartumHealingProblems : Maybe (EverySet PostpartumHealingProblem)
    }


type VaginalExamSign
    = FoulSmellingLochia
    | ExcessiveVaginalBleeding
    | NormalVaginalExam


type GUExamSign
    = EpisiotomyOrPerinealTear
    | RectalHemorrhoids
    | NoGUExamSigns


type PostpartumHealingProblem
    = NormalPostpartumHealing
    | HealingProblemSwelling
    | HealingProblemDischarge
    | HealingProblemReleaseOfSutures
    | HealingProblemHematoma
    | HealingProblemBruising


type alias PrenatalSpecialityCare =
    PrenatalMeasurement SpecialityCareValue


type alias SpecialityCareValue =
    EverySet SpecialityCareSign


type SpecialityCareSign
    = EnrolledToARVProgram
    | EnrolledToNCDProgram
    | NoSpecialityCareSigns


type alias PrenatalPartnerHIVTest =
    PrenatalMeasurement PartnerHIVTestValue


type alias PartnerHIVTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , hivSigns : Maybe (EverySet PrenatalHIVSign)
    }


type alias PrenatalAspirin =
    PrenatalMeasurement AdministrationNote


type alias PrenatalCalcium =
    PrenatalMeasurement AdministrationNote


type alias PrenatalFolate =
    PrenatalMeasurement AdministrationNote


type alias PrenatalFefol =
    PrenatalMeasurement AdministrationNote


type alias PrenatalIron =
    PrenatalMeasurement AdministrationNote


type alias PrenatalMMS =
    PrenatalMeasurement AdministrationNote


type alias PrenatalMebendazole =
    PrenatalMeasurement AdministrationNote



-- ACUTE ILLNESS MEASUREMENTS


type SymptomsGeneralSign
    = BodyAches
    | Chills
    | SymptomGeneralFever
    | Headache
    | NightSweats
    | Lethargy
    | PoorSuck
    | UnableToDrink
    | UnableToEat
    | IncreasedThirst
    | DryMouth
    | SevereWeakness
    | YellowEyes
    | CokeColoredUrine
    | SymptomsGeneralConvulsions
    | SpontaneousBleeding
    | NoSymptomsGeneral


type alias SymptomsGeneralValue =
    { fever : Int
    , chills : Int
    , nightSweats : Int
    , bodyAches : Int
    , headache : Int
    , lethargy : Int
    , poorSuck : Int
    , unableToDrink : Int
    , unableToEat : Int
    , increasedThirst : Int
    , dryMouth : Int
    , severeWeakness : Int
    , yellowEyes : Int
    , cokeColoredUrine : Int
    , convulsions : Int
    , spontaneousBleeding : Int
    }


type alias SymptomsGeneral =
    AcuteIllnessMeasurement (Dict SymptomsGeneralSign Int)


type SymptomsRespiratorySign
    = BloodInSputum
    | Cough
    | NasalCongestion
    | ShortnessOfBreath
    | SoreThroat
    | LossOfSmell
    | StabbingChestPain
    | NoSymptomsRespiratory


type alias SymptomsRespiratory =
    AcuteIllnessMeasurement (Dict SymptomsRespiratorySign Int)


type SymptomsGISign
    = SymptomGIAbdominalPain
    | BloodyDiarrhea
    | Nausea
    | NonBloodyDiarrhea
    | Vomiting
    | NoSymptomsGI


type SymptomsGIDerivedSign
    = IntractableVomiting
    | NoSymptomsGIDerived


type alias SymptomsGIValue =
    { signs : Dict SymptomsGISign Int
    , derivedSigns : EverySet SymptomsGIDerivedSign
    }


type alias SymptomsGI =
    AcuteIllnessMeasurement SymptomsGIValue


type alias AcuteIllnessVitals =
    AcuteIllnessMeasurement VitalsValue


type AcuteFindingsGeneralSign
    = LethargicOrUnconscious
    | AcuteFindingsPoorSuck
    | SunkenEyes
    | PoorSkinTurgor
    | Jaundice
    | NoAcuteFindingsGeneralSigns


type AcuteFindingsRespiratorySign
    = Stridor
    | NasalFlaring
    | SevereWheezing
    | SubCostalRetractions
    | NoAcuteFindingsRespiratorySigns


type alias AcuteFindingsValue =
    { signsGeneral : EverySet AcuteFindingsGeneralSign
    , signsRespiratory : EverySet AcuteFindingsRespiratorySign
    }


type alias AcuteFindings =
    AcuteIllnessMeasurement AcuteFindingsValue


type RapidTestResult
    = RapidTestPositive
    | RapidTestPositiveAndPregnant
    | RapidTestNegative
    | RapidTestIndeterminate
    | RapidTestUnableToRun
    | RapidTestUnableToRunAndPregnant


type alias MalariaTesting =
    AcuteIllnessMeasurement RapidTestResult


type TravelHistorySign
    = COVID19Country
    | NoTravelHistorySigns


type alias TravelHistory =
    AcuteIllnessMeasurement (EverySet TravelHistorySign)


type TreatmentReviewSign
    = FeverPast6Hours
    | FeverPast6HoursHelped
    | MalariaToday
    | MalariaTodayHelped
    | MalariaWithinPastMonth
    | MalariaWithinPastMonthHelped
    | NoTreatmentReviewSigns


type alias TreatmentReview =
    AcuteIllnessMeasurement (EverySet TreatmentReviewSign)


type ExposureSign
    = COVID19Symptoms
    | NoExposureSigns


type alias Exposure =
    AcuteIllnessMeasurement (EverySet ExposureSign)


type IsolationSign
    = PatientIsolated
    | SignOnDoor
    | HealthEducation
    | NoIsolationSigns


type ReasonForNotIsolating
    = NoSpace
    | TooIll
    | CanNotSeparateFromFamily
    | OtherReason
    | IsolationReasonNotApplicable


type alias IsolationValue =
    { signs : EverySet IsolationSign
    , reasonsForNotIsolating : EverySet ReasonForNotIsolating
    }


type alias Isolation =
    AcuteIllnessMeasurement IsolationValue


type alias HCContact =
    AcuteIllnessMeasurement HCContactValue


type alias HCContactValue =
    { signs : EverySet HCContactSign
    , recommendations : EverySet HCRecommendation
    , responsePeriod : EverySet ResponsePeriod
    , ambulanceArrivalPeriod : EverySet ResponsePeriod
    }


type HCContactSign
    = ContactedHealthCenter
    | NoHCContactSigns


type HCRecommendation
    = SendAmbulance
    | HomeIsolation
    | ComeToHealthCenter
    | ChwMonitoring
    | HCRecommendationNotApplicable


type ResponsePeriod
    = LessThan30Min
    | Between30min1Hour
    | Between1Hour2Hour
    | Between2Hour1Day
    | ResponsePeriodNotApplicable


type Call114Sign
    = Call114
    | ContactSite
    | NoCall114Signs


type Recommendation114
    = SendToHealthCenter
    | SendToRRTCenter
    | SendToHospital
    | OtherRecommendation114
    | NoneNoAnswer
    | NoneBusySignal
    | NoneOtherRecommendation114


type RecommendationSite
    = TeamComeToVillage
    | SendToSiteWithForm
    | OtherRecommendationSite
    | NoneSentWithForm
    | NonePatientRefused
    | NoneOtherRecommendationSite
    | RecommendationSiteNotApplicable


type alias Call114Value =
    { signs : EverySet Call114Sign
    , recommendations114 : EverySet Recommendation114
    , recommendationsSite : EverySet RecommendationSite
    }


type alias Call114 =
    AcuteIllnessMeasurement Call114Value


type alias SendToHCValue =
    { signs : EverySet SendToHCSign
    , reasonForNotSendingToHC : ReasonForNonReferral
    }


type SendToHCSign
    = HandReferrerForm
    | ReferToHealthCenter
    | PrenatalAccompanyToHC
    | EnrollToNutritionProgram
    | ReferToNutritionProgram
    | NoSendToHCSigns


type alias SendToHC =
    AcuteIllnessMeasurement SendToHCValue


type MedicationDistributionSign
    = Amoxicillin
    | Aspirin
    | Coartem
    | ORS
    | Zinc
    | LemonJuiceOrHoney
    | Albendazole
    | Mebendezole
    | VitaminA
    | Paracetamol
      -- HIV medication
    | Tenofovir
    | Lamivudine
    | Dolutegravir
    | TDF3TC
      -- Anemia medication and pregnancy supplements.
    | Iron
    | FolicAcid
      -- Pregnancy supplements - in addition to Iron and Folic acid.
    | Calcium
    | MMS
    | Fefol
      -- Gonorhea medication
    | Ceftriaxone
    | Azithromycin
      -- Trichomonas / Bacterial Vaginosis medication
    | Metronidazole
    | NoMedicationDistributionSigns
    | NoMedicationDistributionSignsInitialPhase
    | NoMedicationDistributionSignsRecurrentPhase


type AdministrationNote
    = NonAdministrationLackOfStock
    | NonAdministrationKnownAllergy
    | NonAdministrationPatientDeclined
    | NonAdministrationPatientUnableToAfford
    | NonAdministrationHomeBirth
    | NonAdministrationTooIll
    | NonAdministrationOther
    | AdministeredToday
    | AdministeredPreviously


type MedicationNonAdministrationSign
    = MedicationAmoxicillin AdministrationNote
    | MedicationAspirin AdministrationNote
    | MedicationCoartem AdministrationNote
    | MedicationORS AdministrationNote
    | MedicationZinc AdministrationNote
    | MedicationParacetamol AdministrationNote
    | MedicationMebendezole AdministrationNote
    | MedicationTenofovir AdministrationNote
    | MedicationLamivudine AdministrationNote
    | MedicationDolutegravir AdministrationNote
    | MedicationTDF3TC AdministrationNote
    | MedicationIron AdministrationNote
    | MedicationFolicAcid AdministrationNote
    | MedicationCeftriaxone AdministrationNote
    | MedicationAzithromycin AdministrationNote
    | MedicationMetronidazole AdministrationNote
    | MedicationVitaminA AdministrationNote
    | NoMedicationNonAdministrationSigns


type alias MedicationDistributionValue =
    { distributionSigns : EverySet MedicationDistributionSign
    , nonAdministrationSigns : EverySet MedicationNonAdministrationSign
    }


type alias MedicationDistribution =
    AcuteIllnessMeasurement MedicationDistributionValue


type alias AcuteIllnessMuac =
    AcuteIllnessMeasurement MuacInCm


type ReasonForNonReferral
    = ClientRefused
    | NoAmbulance
    | ClientUnableToAffordFees
    | ClientAlreadyInCare
    | ReasonForNonReferralNotIndicated
    | ReasonForNonReferralOther
    | NoReasonForNonReferral


type TreatmentOngoingSign
    = TakenAsPrescribed
    | MissedDoses
    | FeelingBetter
    | SideEffects
    | NoTreatmentOngoingSign


type ReasonForNotTaking
    = NotTakingAdverseEvent
    | NotTakingNoMoney
    | NotTakingMemoryProblems
    | NotTakingTreatmentNotStarted
    | NotTakingOther
    | NoReasonForNotTakingSign


type ReasonForNotProvidingHealthEducation
    = PatientNeedsEmergencyReferral
    | ReceivedEmergencyCase
    | LackOfAppropriateEducationUserGuide
    | PatientRefused
    | PatientTooIll
    | NoReasonForNotProvidingHealthEducation


type AdverseEvent
    = AdverseEventRashOrItching
    | AdverseEventFever
    | AdverseEventDiarrhea
    | AdverseEventVomiting
    | AdverseEventFatigue
    | AdverseEventOther
    | NoAdverseEvent


type alias TreatmentOngoingValue =
    { signs : EverySet TreatmentOngoingSign
    , reasonForNotTaking : ReasonForNotTaking
    , missedDoses : Int
    , adverseEvents : EverySet AdverseEvent
    }


type alias TreatmentOngoing =
    AcuteIllnessMeasurement TreatmentOngoingValue


type alias AcuteIllnessCoreExam =
    AcuteIllnessMeasurement AcuteIllnessCoreExamValue


type alias AcuteIllnessCoreExamValue =
    { heart : EverySet HeartCPESign
    , lungs : EverySet LungsCPESign
    }


type AcuteIllnessDangerSign
    = DangerSignConditionNotImproving
    | DangerSignUnableDrinkSuck
    | DangerSignVomiting
    | DangerSignConvulsions
    | DangerSignLethargyUnconsciousness
    | DangerSignRespiratoryDistress
    | DangerSignSpontaneousBleeding
    | DangerSignBloodyDiarrhea
    | DangerSignNewSkinRash
    | NoAcuteIllnessDangerSign


type alias AcuteIllnessDangerSigns =
    AcuteIllnessMeasurement (EverySet AcuteIllnessDangerSign)


type alias AcuteIllnessNutrition =
    AcuteIllnessMeasurement (EverySet ChildNutritionSign)


type alias HealthEducationValue =
    { signs : EverySet HealthEducationSign
    , reasonForNotProvidingHealthEducation : ReasonForNotProvidingHealthEducation
    }


type alias HealthEducation =
    AcuteIllnessMeasurement HealthEducationValue


type HealthEducationSign
    = MalariaPrevention
    | NoHealthEducationSigns


type alias AcuteIllnessFollowUp =
    AcuteIllnessMeasurement AcuteIllnessFollowUpValue


type alias AcuteIllnessFollowUpValue =
    { options : EverySet FollowUpOption
    , resolutionDate : Maybe NominalDate
    , diagnosis : Maybe AcuteIllnessDiagnosis
    }


type alias CovidTesting =
    AcuteIllnessMeasurement CovidTestingValue


type alias CovidTestingValue =
    { result : RapidTestResult
    , administrationNote : Maybe AdministrationNote
    }


type alias AcuteIllnessContactsTracing =
    AcuteIllnessMeasurement (List ContactTraceItem)


type alias AcuteIllnessTraceContact =
    AcuteIllnessMeasurement ContactTraceItem


type alias ContactTraceItem =
    { personId : PersonId
    , firstName : String
    , secondName : String
    , gender : Gender
    , phoneNumber : String
    , contactDate : NominalDate
    , resolutionDate : NominalDate
    , lastFollowUpDate : Maybe NominalDate
    , generalSigns : Maybe (EverySet SymptomsGeneralSign)
    , respiratorySigns : Maybe (EverySet SymptomsRespiratorySign)
    , giSigns : Maybe (EverySet SymptomsGISign)
    , traceOutcome : Maybe TraceOutcome
    }


type Gender
    = Female
    | Male


type TraceOutcome
    = OutcomeNoAnswer
    | OutcomeWrongContactInfo
    | OutcomeDeclinedFollowUp
    | OutcomeNoSymptoms
    | OutcomeReferredToHC



-- WELL CHILD MEASUREMENTS


type alias WellChildSymptomsReview =
    WellChildMeasurement (EverySet WellChildSymptom)


type WellChildSymptom
    = SymptomBreathingProblems
    | SymptomConvulsions
    | SymptomLethargyOrUnresponsiveness
    | SymptomDiarrhea
    | SymptomVomiting
    | SymptomUmbilicalCordRedness
    | SymptomStiffNeckOrBulgingFontanelle
    | SymptomSevereEdema
    | SymptomPalmoplantarPallor
    | SymptomHistoryOfFever
    | SymptomBabyTiresQuicklyWhenFeeding
    | SymptomCoughingOrTearingWhileFeeding
    | SymptomRigidMusclesOrJawClenchingPreventingFeeding
    | ExcessiveSweatingWhenFeeding
    | NoWellChildSymptoms


type alias WellChildVitals =
    WellChildMeasurement VitalsValue


type alias WellChildHeight =
    WellChildMeasurement HeightInCm


type alias WellChildMuac =
    WellChildMeasurement MuacInCm


type alias WellChildNutrition =
    WellChildMeasurement NutritionValue


type alias WellChildPhoto =
    WellChildMeasurement ImageUrl


type alias WellChildWeight =
    WellChildMeasurement WeightInKg


type alias WellChildSendToHC =
    WellChildMeasurement SendToHCValue


type alias WellChildHealthEducation =
    WellChildMeasurement HealthEducationValue


type alias WellChildContributingFactors =
    WellChildMeasurement (EverySet ContributingFactorsSign)


type alias WellChildFollowUp =
    WellChildMeasurement NutritionFollowUpValue


type alias WellChildHeadCircumference =
    WellChildMeasurement HeadCircumferenceValue


type alias HeadCircumferenceValue =
    { headCircumference : HeadCircumferenceInCm
    , notes : EverySet MeasurementNote
    }


type HeadCircumferenceInCm
    = HeadCircumferenceInCm Float


type MeasurementNote
    = NoteNotTaken
    | NoMeasurementNotes


type alias WellChildECD =
    WellChildMeasurement (EverySet ECDSign)


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


type VaccineType
    = WellChildVaccine WellChildVaccineType
    | PrenatalVaccine PrenatalVaccineType


type WellChildVaccineType
    = VaccineBCG
    | VaccineOPV
    | VaccineDTP
    | VaccineDTPStandalone
    | VaccinePCV13
    | VaccineRotarix
    | VaccineIPV
    | VaccineMR
    | VaccineHPV


type PrenatalVaccineType
    = VaccineTetanus


type VaccineDose
    = VaccineDoseFirst
    | VaccineDoseSecond
    | VaccineDoseThird
    | VaccineDoseFourth
    | VaccineDoseFifth


type alias WellChildMebendezole =
    WellChildMeasurement AdministrationNote


type alias WellChildVitaminA =
    WellChildMeasurement AdministrationNote


type alias WellChildAlbendazole =
    WellChildMeasurement AdministrationNote


type alias WellChildPregnancySummary =
    WellChildMeasurement PregnancySummaryValue


type alias PregnancySummaryValue =
    { expectedDateConcluded : NominalDate
    , deliveryComplications : EverySet DeliveryComplication
    , signs : EverySet PregnancySummarySign
    , apgarOneMin : Maybe Float
    , apgarFiveMin : Maybe Float
    , birthWeight : Maybe WeightInGrm
    , birthLength : Maybe HeightInCm
    , birthDefects : EverySet BirthDefect
    }


type PregnancySummarySign
    = ApgarScores
    | BirthLength
    | NoPregnancySummarySigns


type DeliveryComplication
    = ComplicationGestationalDiabetes
    | ComplicationEmergencyCSection
    | ComplicationPreclampsia
    | ComplicationMaternalHemmorhage
    | ComplicationHiv
    | ComplicationMaternalDeath
    | ComplicationOther
    | NoDeliveryComplications


type BirthDefect
    = DefectBirthInjury
    | DefectCleftLipWithCleftPalate
    | DefectCleftPalate
    | DefectClubFoot
    | DefectMacrocephaly
    | DefectGastroschisis
    | DefectHearingLoss
    | DefectUndescendedTestes
    | DefectHypospadias
    | DefectInguinalHernia
    | DefectMicrocephaly
    | DefectNeuralTubes
    | DefectDownSyndrome
    | DefectCongenitalHeart
    | DefectVentricalSeptal
    | DefectPulmonaryValveAtresiaAndStenosis
    | NoBirthDefects


type alias WellChildNextVisit =
    WellChildMeasurement NextVisitValue


type alias NextVisitValue =
    { immunisationDate : Maybe NominalDate
    , asapImmunisationDate : Maybe NominalDate
    , pediatricVisitDate : Maybe NominalDate
    , resolutionDate : Maybe NominalDate
    }


type alias WellChildBCGImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildDTPImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildDTPStandaloneImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildHPVImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildIPVImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildMRImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildOPVImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildPCV13Immunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildRotarixImmunisation =
    WellChildMeasurement VaccinationValue


type alias VaccinationValue =
    { administeredDoses : EverySet VaccineDose
    , administrationDates : EverySet NominalDate
    , administrationNote : AdministrationNote
    }


type alias WellChildNCDA =
    WellChildMeasurement NCDAValue


type alias WellChildFeeding =
    WellChildMeasurement NutritionFeedingValue


type alias WellChildHygiene =
    WellChildMeasurement NutritionHygieneValue


type alias WellChildFoodSecurity =
    WellChildMeasurement NutritionFoodSecurityValue


type alias WellChildCaring =
    WellChildMeasurement NutritionCaringValue



-- NCD MEASUREMENTS


type alias NCDCoMorbidities =
    NCDMeasurement NCDCoMorbiditiesValue


type alias NCDCoMorbiditiesValue =
    EverySet MedicalCondition


type MedicalCondition
    = MedicalConditionHIV
    | MedicalConditionDiabetes
    | MedicalConditionKidneyDisease
    | MedicalConditionPregnancy
    | MedicalConditionHypertension
    | MedicalConditionGestationalDiabetes
    | MedicalConditionPregnancyRelatedHypertension
    | MedicalConditionNeuropathy
    | MedicalConditionRentalComplications
    | MedicalConditionMalaria
    | MedicalConditionTuberculosis
    | MedicalConditionHepatitisB
    | MedicalConditionSyphilis
    | MedicalConditionEyeComplications
    | MedicalConditionAnemia
    | MedicalConditionOther
    | NoMedicalConditions


type alias NCDCoreExam =
    NCDMeasurement CorePhysicalExamValue


type alias NCDCreatinineTest =
    NCDMeasurement CreatinineTestValue


type alias CreatinineTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , creatinineResult : Maybe Float
    , bunResult : Maybe Float
    }


type alias NCDDangerSigns =
    NCDMeasurement NCDDangerSignsValue


type alias NCDDangerSignsValue =
    EverySet NCDDangerSign


type NCDDangerSign
    = Dyspnea
    | VisionChanges
    | ChestPain
    | FlankPain
    | Hematuria
    | SevereHeadaches
    | LossOfConciousness
    | NoNCDDangerSigns


type alias NCDFamilyHistory =
    NCDMeasurement NCDFamilyHistoryValue


type alias NCDFamilyHistoryValue =
    { signs : EverySet NCDFamilyHistorySign
    , hypertensionPredecessors : Maybe (EverySet Predecessor)
    , heartProblemPredecessors : Maybe (EverySet Predecessor)
    , diabetesPredecessors : Maybe (EverySet Predecessor)
    }


type NCDFamilyHistorySign
    = SignHypertensionHistory
    | SignHeartProblemHistory
    | SignDiabetesHistory
    | NoNCDFamilyHistorySigns


type Predecessor
    = PredecessorFather
    | PredecessorMother
    | PredecessorGrandFather
    | PredecessorGrandMother
    | NoPredecessors


type alias NCDFamilyPlanning =
    NCDMeasurement NCDFamilyPlanningValue


type alias NCDFamilyPlanningValue =
    EverySet FamilyPlanningSign


type alias NCDHealthEducation =
    NCDMeasurement NCDHealthEducationValue


type alias NCDHealthEducationValue =
    EverySet NCDHealthEducationSign


type NCDHealthEducationSign
    = EducationHypertension
    | NoNCDHealthEducationSigns


type alias NCDHIVTest =
    NCDMeasurement HIVTestValue


type alias NCDLiverFunctionTest =
    NCDMeasurement LiverFunctionTestValue


type alias LiverFunctionTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , altResult : Maybe Float
    , astResult : Maybe Float
    }


type alias NCDMedicationDistribution =
    NCDMeasurement NCDMedicationDistributionValue


type alias NCDMedicationDistributionValue =
    { recommendedTreatmentSigns : EverySet RecommendedTreatmentSign
    , guidanceSigns : EverySet NCDGuidanceSign
    }


type NCDGuidanceSign
    = ReturnInOneMonth
    | NoNCDGuidanceSigns


type alias NCDMedicationHistory =
    NCDMeasurement NCDMedicationHistoryValue


type alias NCDMedicationHistoryValue =
    { medicationsCausingHypertension : EverySet MedicationCausingHypertension
    , medicationsTreatingHypertension : EverySet MedicationTreatingHypertension
    , medicationsTreatingDiabetes : EverySet MedicationTreatingDiabetes
    }


type MedicationCausingHypertension
    = MedicationOestrogens
    | MedicationSteroids
    | MedicationAmitriptyline
    | MedicationIbuprofen
    | NoMedicationCausingHypertension


type MedicationTreatingHypertension
    = MedicationAceInhibitors
    | MedicationARBs
    | MedicationHCTZ
    | MedicationCalciumChannelBlockers
    | MedicationMethyldopa
    | MedicationBetaBlockers
    | MedicationHydralazine
    | NoMedicationTreatingHypertension


type MedicationTreatingDiabetes
    = MedicationMetformin
    | MedicationGlibenclamide
    | MedicationInsulin
    | NoMedicationTreatingDiabetes


type alias NCDOutsideCare =
    NCDMeasurement (OutsideCareValue MedicalCondition)


type alias NCDPregnancyTest =
    NCDMeasurement PregnancyTestValue


type alias PregnancyTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , testResult : Maybe TestResult
    }


type alias NCDRandomBloodSugarTest =
    NCDMeasurement (RandomBloodSugarTestValue NCDEncounterId)


type alias NCDReferral =
    NCDMeasurement ReferralValue


type alias ReferralValue =
    { referralSigns : EverySet ReferToFacilitySign
    , nonReferralReasons : Maybe (EverySet NonReferralSign)
    }


type alias NCDSocialHistory =
    NCDMeasurement NCDSocialHistoryValue


type alias NCDSocialHistoryValue =
    { signs : EverySet NCDSocialHistorySign
    , foodGroup : FoodGroup
    , beveragesPerWeek : Maybe Int
    , cigarettesPerWeek : Maybe Int
    }


type NCDSocialHistorySign
    = SignDrinkAlcohol
    | SignSmokeCigarettes
    | SignConsumeSalt
    | SignDifficult4TimesAYear
    | SignHelpWithTreatmentAtHome
    | NoNCDSocialHistorySigns


type FoodGroup
    = FoodGroupVegetables
    | FoodGroupCarbohydrates
    | FoodGroupProtein


type alias NCDSymptomReview =
    NCDMeasurement NCDSymptomReviewValue


type alias NCDSymptomReviewValue =
    { group1Symptoms : EverySet NCDGroup1Symptom
    , group2Symptoms : EverySet NCDGroup2Symptom
    , painSymptoms : EverySet NCDPainSymptom
    }


type NCDGroup1Symptom
    = SwellingInLegs
    | UrinaryFrequency
    | Anxiety
    | WeightLoss
    | Palpitations
    | Tremor
    | SwellingInFace
    | SwellingInAbdomen
    | DizzinessWithChangingPosition
    | MildHeadache
    | NoNCDGroup1Symptoms


type NCDPainSymptom
    = PainFlank
    | PainLowerBack
    | PainFeet
    | PainNeck
    | PainAbdomen
    | NoNCDPainSymptoms


type NCDGroup2Symptom
    = WeaknessOfOneSideOfTheBody
    | ProblemsWithWalking
    | ProblemsWithTalking
    | DecreasedVision
    | BlurryVision
    | IncreasedFatigueWithDailyActivities
    | ShortOfBreathWhenLayingDown
    | ShortOfBreathAtNight
    | KidneyProblems
    | NCDIncreasedThirst
    | NoNCDGroup2Symptoms


type alias NCDUrineDipstickTest =
    NCDMeasurement UrineDipstickTestValue


type alias NCDVitals =
    NCDMeasurement VitalsValue


type alias NCDLabsResults =
    NCDMeasurement LabsResultsValue


type alias NCDLipidPanelTest =
    NCDMeasurement LipidPanelTestValue


type alias LipidPanelTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate

    -- Indicates what unit of measurement was used while results were recorded.
    , unitOfMeasurement : Maybe UnitOfMeasurement

    -- All results are stored in mg/dL unit.
    , totalCholesterolResult : Maybe Float
    , ldlCholesterolResult : Maybe Float
    , hdlCholesterolResult : Maybe Float
    , triglyceridesResult : Maybe Float
    }


type UnitOfMeasurement
    = UnitMmolL
    | UnitMgdL


type alias NCDHbA1cTest =
    NCDMeasurement HbA1cTestValue


type alias HbA1cTestValue =
    { executionNote : TestExecutionNote
    , executionDate : Maybe NominalDate
    , hba1cResult : Maybe Float
    }



-- Child Scorecard measurements.


type alias ChildScoreboardNCDA =
    ChildScoreboardMeasurement NCDAValue


type alias ChildScoreboardBCGImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardDTPImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardDTPStandaloneImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardIPVImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardMRImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardOPVImmunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardPCV13Immunisation =
    ChildScoreboardMeasurement VaccinationValue


type alias ChildScoreboardRotarixImmunisation =
    ChildScoreboardMeasurement VaccinationValue



-- Tuberculosis:


type alias TuberculosisDiagnostics =
    TuberculosisMeasurement TuberculosisDiagnosticsValue


type alias TuberculosisDiagnosticsValue =
    TuberculosisDiagnosis


type TuberculosisDiagnosis
    = TuberculosisPulmonary
    | TuberculosisExtrapulmonary
    | NoTuberculosis


type alias TuberculosisDOT =
    TuberculosisMeasurement TuberculosisDOTValue


type alias TuberculosisDOTValue =
    { sign : TuberculosisDOTSign
    , medicationDistributionSign : TuberculosisDOTSign
    }


type TuberculosisDOTSign
    = DOTPositive
    | DOTNegativeTakenToday
    | DOTNegativeUnavailable
    | DOTNegativeSideEffects
    | DOTNegativePatientRefused
    | DOTNegativeNotIndicated


type alias TuberculosisFollowUp =
    TuberculosisMeasurement FollowUpValue


type alias FollowUpValue =
    { options : EverySet FollowUpOption
    , resolutionDate : Maybe NominalDate
    }


type alias TuberculosisHealthEducation =
    TuberculosisMeasurement TuberculosisHealthEducationValue


type alias TuberculosisHealthEducationValue =
    EverySet TuberculosisHealthEducationSign


type TuberculosisHealthEducationSign
    = EducationFollowUpTesting
    | NoTuberculosisHealthEducationSigns


type alias TuberculosisMedication =
    TuberculosisMeasurement TuberculosisMedicationValue


type alias TuberculosisMedicationValue =
    EverySet TuberculosisPrescribedMedication


type TuberculosisPrescribedMedication
    = MedicationRHZE
    | MedicationRH
    | MedicationOther
    | TuberculosisMedicationsNotChanged
    | NoTuberculosisPrescribedMedications


type alias TuberculosisReferral =
    TuberculosisMeasurement SendToHCValue


type alias TuberculosisSymptomReview =
    TuberculosisMeasurement TuberculosisSymptomReviewValue


type alias TuberculosisSymptomReviewValue =
    EverySet TuberculosisSymptom


type TuberculosisSymptom
    = TuberculosisSymptomNightSweats
    | TuberculosisSymptomBloodInSputum
    | TuberculosisSymptomWeightLoss
    | TuberculosisSymptomSevereFatigue
    | NoTuberculosisSymptoms


type alias TuberculosisTreatmentReview =
    TuberculosisMeasurement TreatmentOngoingValue



-- HIV:


type alias HIVDiagnostics =
    HIVMeasurement HIVDiagnosticsValue


type alias HIVDiagnosticsValue =
    { signs : EverySet HIVDiagnosisSign
    , positiveResultDate : Maybe NominalDate
    , testResult : Maybe TestResult
    }


type HIVDiagnosisSign
    = HIVResultPositiveReported
    | HIVResultPositiveKnown
    | HIVResultDateEstimated
    | HIVTestRun
    | NoHIVDiagnosisSigns


type alias HIVFollowUp =
    HIVMeasurement FollowUpValue


type alias HIVHealthEducation =
    HIVMeasurement HIVHealthEducationValue


type alias HIVHealthEducationValue =
    EverySet HIVHealthEducationSign


type HIVHealthEducationSign
    = EducationPositiveResult
    | EducationSaferSexPractices
    | EducationEncouragedPartnerTesting
    | EducationFamilyPlanningOptions
    | NoHIVHealthEducationSigns


type alias HIVMedication =
    HIVMeasurement HIVMedicationValue


type alias HIVMedicationValue =
    EverySet HIVPrescribedMedication


type HIVPrescribedMedication
    = HIVMedicationDolutegravirLamivudineTenofovir
    | HIVMedicationAtazanavirRitonavir
    | HIVMedicationDolutegravir
    | HIVMedicationAbacavirLamivudine
    | HIVMedicationLamivudineTenofovir
    | HIVMedicationZidovudine
    | HIVMedicationLamivudineZidovudineNevirapine
    | HIVMedicationEfavirenzLamivudineTenofovir
    | HIVMedicationLamivudineZidovudine
    | HIVMedicationLopinavirRitonavir
    | HIVMedicationDarunavirRitonavir
    | HIVMedicationDarunavirCobicistat
    | HIVMedicationRaltegravir
    | HIVMedicationEfavirenz
    | HIVMedicationNevirapine
    | HIVMedicationEtravirine
    | HIVMedicationTenofovir
    | HIVMedicationLamivudine
    | HIVMedicationAbacavir
    | HIVMedicationBactrim
    | HIVMedicationTrimethoprimSulfamethoxazole
    | HIVMedicationCoTrimoxazoleTablets
    | HIVMedicationCoTrimoxazoleOralSuspension
    | HIVMedicationsNotChanged
    | NoHIVPrescribedMedications


type alias HIVReferral =
    HIVMeasurement SendToHCValue


type alias HIVSymptomReview =
    HIVMeasurement HIVSymptomReviewValue


type alias HIVSymptomReviewValue =
    EverySet HIVSymptom


type HIVSymptom
    = HIVSymptomFever
    | HIVSymptomFatigue
    | HIVSymptomSwollenLymphNodes
    | HIVSymptomSoreThroat
    | HIVSymptomRash
    | HIVSymptomMuscleJointPain
    | HIVSymptomHeadache
    | HIVSymptomSevereAbdominalPain
    | HIVSymptomNightSweats
    | HIVSymptomDiarrhea
    | HIVSymptomWeightLoss
    | HIVSymptomCoughingUpBlood
    | HIVSymptomHairLoss
    | HIVSymptomMouthUlcers
    | HIVSymptomDifficultyBreathing
    | HIVSymptomVomiting
    | NoHIVSymptoms


type alias HIVTreatmentReview =
    HIVMeasurement TreatmentOngoingValue



-- Stock Management:


type alias StockUpdate =
    { nurse : NurseId
    , dateMeasured : NominalDate
    , updateType : StockUpdateType
    , quantity : Int
    , dateRecorded : NominalDate
    , dateExpires : Maybe NominalDate
    , batchNumber : Maybe String
    , supplier : Maybe StockSupplier
    , notes : Maybe String
    , correctionReason : Maybe StockCorrectionReason
    , healthCenter : HealthCenterId
    , deleted : Bool
    , shard : Maybe HealthCenterId
    , signature : ImageUrl
    }


type StockUpdateType
    = UpdateReceivingSupplies
    | UpdateCorrection


type StockSupplier
    = SupplierMOH
    | SupplierRBC
    | SupplierUNICEF
    | SupplierRMSCentral
    | SupplierRMSDistrict
    | SupplierBUFMAR


type StockCorrectionReason
    = ReasonInputError
    | ReasonExpiration
    | ReasonMissing
    | ReasonOther



-- LISTS OF MEASUREMENTS


type alias MotherMeasurementList =
    { attendances : Dict AttendanceId Attendance
    , familyPlannings : Dict FamilyPlanningId FamilyPlanning
    , consents : Dict ParticipantConsentId ParticipantConsent
    , lactations : Dict LactationId Lactation
    , fbfs : Dict MotherFbfId Fbf
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { attendances = Dict.empty
    , familyPlannings = Dict.empty
    , consents = Dict.empty
    , lactations = Dict.empty
    , fbfs = Dict.empty
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes.

-}
type alias ChildMeasurementList =
    { heights : Dict HeightId Height
    , muacs : Dict MuacId Muac
    , nutritions : Dict ChildNutritionId ChildNutrition
    , photos : Dict PhotoId Photo
    , weights : Dict WeightId Weight
    , counselingSessions : Dict CounselingSessionId CounselingSession
    , fbfs : Dict ChildFbfId Fbf
    , contributingFactors : Dict ContributingFactorsId ContributingFactors
    , followUp : Dict FollowUpId FollowUp
    , healthEducation : Dict GroupHealthEducationId GroupHealthEducation
    , sendToHC : Dict GroupSendToHCId GroupSendToHC
    , ncda : Dict GroupNCDAId GroupNCDA
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = Dict.empty
    , muacs = Dict.empty
    , nutritions = Dict.empty
    , photos = Dict.empty
    , weights = Dict.empty
    , counselingSessions = Dict.empty
    , fbfs = Dict.empty
    , contributingFactors = Dict.empty
    , followUp = Dict.empty
    , healthEducation = Dict.empty
    , sendToHC = Dict.empty
    , ncda = Dict.empty
    }


{-| This type just organizes historical measurements by mother or child, for
our convenience.
-}
type alias HistoricalMeasurements =
    { mothers : Dict PersonId MotherMeasurementList
    , children : Dict PersonId ChildMeasurementList
    }


emptyHistoricalMeasurements : HistoricalMeasurements
emptyHistoricalMeasurements =
    { mothers = Dict.empty
    , children = Dict.empty
    }



-- ONE OF EACH KIND OF MEASUREMENT


{-| A set of prenatal measurements that correspond to the same Prenatal encounter.
-}
type alias PrenatalMeasurements =
    { breastExam : Maybe ( BreastExamId, BreastExam )
    , corePhysicalExam : Maybe ( CorePhysicalExamId, CorePhysicalExam )
    , dangerSigns : Maybe ( DangerSignsId, DangerSigns )
    , lastMenstrualPeriod : Maybe ( LastMenstrualPeriodId, LastMenstrualPeriod )
    , medicalHistory : Maybe ( MedicalHistoryId, MedicalHistory )
    , medication : Maybe ( MedicationId, Medication )
    , obstetricalExam : Maybe ( ObstetricalExamId, ObstetricalExam )
    , obstetricHistory : Maybe ( ObstetricHistoryId, ObstetricHistory )
    , obstetricHistoryStep2 : Maybe ( ObstetricHistoryStep2Id, ObstetricHistoryStep2 )
    , familyPlanning : Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning )
    , nutrition : Maybe ( PrenatalNutritionId, PrenatalNutrition )
    , malariaPrevention : Maybe ( MalariaPreventionId, MalariaPrevention )
    , socialHistory : Maybe ( SocialHistoryId, SocialHistory )
    , vitals : Maybe ( VitalsId, Vitals )
    , prenatalPhoto : Maybe ( PrenatalPhotoId, PrenatalPhoto )
    , birthPlan : Maybe ( BirthPlanId, BirthPlan )
    , pregnancyTest : Maybe ( PregnancyTestId, PregnancyTest )
    , healthEducation : Maybe ( PrenatalHealthEducationId, PrenatalHealthEducation )
    , followUp : Maybe ( PrenatalFollowUpId, PrenatalFollowUp )
    , sendToHC : Maybe ( PrenatalSendToHCId, PrenatalSendToHC )
    , appointmentConfirmation : Maybe ( PrenatalAppointmentConfirmationId, PrenatalAppointmentConfirmation )
    , bloodGpRsTest : Maybe ( PrenatalBloodGpRsTestId, PrenatalBloodGpRsTest )
    , hemoglobinTest : Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )
    , hepatitisBTest : Maybe ( PrenatalHepatitisBTestId, PrenatalHepatitisBTest )
    , hivTest : Maybe ( PrenatalHIVTestId, PrenatalHIVTest )
    , malariaTest : Maybe ( PrenatalMalariaTestId, PrenatalMalariaTest )
    , randomBloodSugarTest : Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )
    , syphilisTest : Maybe ( PrenatalSyphilisTestId, PrenatalSyphilisTest )
    , urineDipstickTest : Maybe ( PrenatalUrineDipstickTestId, PrenatalUrineDipstickTest )
    , labsResults : Maybe ( PrenatalLabsResultsId, PrenatalLabsResults )
    , medicationDistribution : Maybe ( PrenatalMedicationDistributionId, PrenatalMedicationDistribution )
    , symptomReview : Maybe ( PrenatalSymptomReviewId, PrenatalSymptomReview )
    , outsideCare : Maybe ( PrenatalOutsideCareId, PrenatalOutsideCare )
    , hivPCRTest : Maybe ( PrenatalHIVPCRTestId, PrenatalHIVPCRTest )
    , mentalHealth : Maybe ( PrenatalMentalHealthId, PrenatalMentalHealth )
    , tetanusImmunisation : Maybe ( PrenatalTetanusImmunisationId, PrenatalTetanusImmunisation )
    , breastfeeding : Maybe ( PrenatalBreastfeedingId, PrenatalBreastfeeding )
    , guExam : Maybe ( PrenatalGUExamId, PrenatalGUExam )
    , specialityCare : Maybe ( PrenatalSpecialityCareId, PrenatalSpecialityCare )
    , partnerHIVTest : Maybe ( PrenatalPartnerHIVTestId, PrenatalPartnerHIVTest )
    , aspirin : Maybe ( PrenatalAspirinId, PrenatalAspirin )
    , calcium : Maybe ( PrenatalCalciumId, PrenatalCalcium )
    , fefol : Maybe ( PrenatalFefolId, PrenatalFefol )
    , folate : Maybe ( PrenatalFolateId, PrenatalFolate )
    , iron : Maybe ( PrenatalIronId, PrenatalIron )
    , mms : Maybe ( PrenatalMMSId, PrenatalMMS )
    , mebendazole : Maybe ( PrenatalMebendazoleId, PrenatalMebendazole )
    }


emptyPrenatalMeasurements : PrenatalMeasurements
emptyPrenatalMeasurements =
    { breastExam = Nothing
    , corePhysicalExam = Nothing
    , dangerSigns = Nothing
    , lastMenstrualPeriod = Nothing
    , medicalHistory = Nothing
    , medication = Nothing
    , obstetricalExam = Nothing
    , obstetricHistory = Nothing
    , obstetricHistoryStep2 = Nothing
    , familyPlanning = Nothing
    , nutrition = Nothing
    , malariaPrevention = Nothing
    , socialHistory = Nothing
    , vitals = Nothing
    , prenatalPhoto = Nothing
    , birthPlan = Nothing
    , pregnancyTest = Nothing
    , healthEducation = Nothing
    , followUp = Nothing
    , sendToHC = Nothing
    , appointmentConfirmation = Nothing
    , bloodGpRsTest = Nothing
    , hemoglobinTest = Nothing
    , hepatitisBTest = Nothing
    , hivTest = Nothing
    , malariaTest = Nothing
    , randomBloodSugarTest = Nothing
    , syphilisTest = Nothing
    , urineDipstickTest = Nothing
    , labsResults = Nothing
    , medicationDistribution = Nothing
    , symptomReview = Nothing
    , outsideCare = Nothing
    , hivPCRTest = Nothing
    , mentalHealth = Nothing
    , tetanusImmunisation = Nothing
    , breastfeeding = Nothing
    , guExam = Nothing
    , specialityCare = Nothing
    , partnerHIVTest = Nothing
    , aspirin = Nothing
    , calcium = Nothing
    , fefol = Nothing
    , folate = Nothing
    , iron = Nothing
    , mms = Nothing
    , mebendazole = Nothing
    }


{-| A set of Nutrition measurements that correspond to the same Nutrition encounter.
-}
type alias NutritionMeasurements =
    { muac : Maybe ( NutritionMuacId, NutritionMuac )
    , height : Maybe ( NutritionHeightId, NutritionHeight )
    , nutrition : Maybe ( NutritionNutritionId, NutritionNutrition )
    , photo : Maybe ( NutritionPhotoId, NutritionPhoto )
    , weight : Maybe ( NutritionWeightId, NutritionWeight )
    , sendToHC : Maybe ( NutritionSendToHCId, NutritionSendToHC )
    , healthEducation : Maybe ( NutritionHealthEducationId, NutritionHealthEducation )
    , contributingFactors : Maybe ( NutritionContributingFactorsId, NutritionContributingFactors )
    , followUp : Maybe ( NutritionFollowUpId, NutritionFollowUp )
    , ncda : Maybe ( NutritionNCDAId, NutritionNCDA )
    }


{-| A set of Acute Illness measurements that correspond to the same Acute Illness encounter.
-}
type alias AcuteIllnessMeasurements =
    { symptomsGeneral : Maybe ( SymptomsGeneralId, SymptomsGeneral )
    , symptomsRespiratory : Maybe ( SymptomsRespiratoryId, SymptomsRespiratory )
    , symptomsGI : Maybe ( SymptomsGIId, SymptomsGI )
    , vitals : Maybe ( AcuteIllnessVitalsId, AcuteIllnessVitals )
    , acuteFindings : Maybe ( AcuteFindingsId, AcuteFindings )
    , malariaTesting : Maybe ( MalariaTestingId, MalariaTesting )
    , travelHistory : Maybe ( TravelHistoryId, TravelHistory )
    , exposure : Maybe ( ExposureId, Exposure )
    , isolation : Maybe ( IsolationId, Isolation )
    , hcContact : Maybe ( HCContactId, HCContact )
    , call114 : Maybe ( Call114Id, Call114 )
    , treatmentReview : Maybe ( TreatmentReviewId, TreatmentReview )
    , sendToHC : Maybe ( SendToHCId, SendToHC )
    , medicationDistribution : Maybe ( MedicationDistributionId, MedicationDistribution )
    , muac : Maybe ( AcuteIllnessMuacId, AcuteIllnessMuac )
    , treatmentOngoing : Maybe ( TreatmentOngoingId, TreatmentOngoing )
    , dangerSigns : Maybe ( AcuteIllnessDangerSignsId, AcuteIllnessDangerSigns )
    , nutrition : Maybe ( AcuteIllnessNutritionId, AcuteIllnessNutrition )
    , healthEducation : Maybe ( HealthEducationId, HealthEducation )
    , followUp : Maybe ( AcuteIllnessFollowUpId, AcuteIllnessFollowUp )
    , coreExam : Maybe ( AcuteIllnessCoreExamId, AcuteIllnessCoreExam )
    , covidTesting : Maybe ( CovidTestingId, CovidTesting )
    , contactsTracing : Maybe ( AcuteIllnessContactsTracingId, AcuteIllnessContactsTracing )
    }


{-| A set of measurements that represent follow ups needed for certain Healh Center.
-}
type alias FollowUpMeasurements =
    { nutritionGroup : Dict FollowUpId FollowUp
    , nutritionIndividual : Dict NutritionFollowUpId NutritionFollowUp
    , acuteIllness : Dict AcuteIllnessFollowUpId AcuteIllnessFollowUp
    , prenatal : Dict PrenatalFollowUpId PrenatalFollowUp
    , wellChild : Dict WellChildFollowUpId WellChildFollowUp
    , tuberculosis : Dict TuberculosisFollowUpId TuberculosisFollowUp
    , hiv : Dict HIVFollowUpId HIVFollowUp
    , traceContacts : Dict AcuteIllnessTraceContactId AcuteIllnessTraceContact
    , prenatalLabs : Dict PrenatalLabsResultsId PrenatalLabsResults
    , ncdLabs : Dict NCDLabsResultsId NCDLabsResults
    , nextVisit : Dict WellChildNextVisitId WellChildNextVisit
    }


{-| A set of Home Visit measurements that correspond to the same Home Visit encounter.
-}
type alias HomeVisitMeasurements =
    { feeding : Maybe ( NutritionFeedingId, NutritionFeeding )
    , hygiene : Maybe ( NutritionHygieneId, NutritionHygiene )
    , foodSecurity : Maybe ( NutritionFoodSecurityId, NutritionFoodSecurity )
    , caring : Maybe ( NutritionCaringId, NutritionCaring )
    }


{-| A set of Well Child measurements that correspond to the same Well Child encounter.
-}
type alias WellChildMeasurements =
    { pregnancySummary : Maybe ( WellChildPregnancySummaryId, WellChildPregnancySummary )
    , symptomsReview : Maybe ( WellChildSymptomsReviewId, WellChildSymptomsReview )
    , vitals : Maybe ( WellChildVitalsId, WellChildVitals )
    , height : Maybe ( WellChildHeightId, WellChildHeight )
    , muac : Maybe ( WellChildMuacId, WellChildMuac )
    , nutrition : Maybe ( WellChildNutritionId, WellChildNutrition )
    , photo : Maybe ( WellChildPhotoId, WellChildPhoto )
    , weight : Maybe ( WellChildWeightId, WellChildWeight )
    , contributingFactors : Maybe ( WellChildContributingFactorsId, WellChildContributingFactors )
    , healthEducation : Maybe ( WellChildHealthEducationId, WellChildHealthEducation )
    , followUp : Maybe ( WellChildFollowUpId, WellChildFollowUp )
    , sendToHC : Maybe ( WellChildSendToHCId, WellChildSendToHC )
    , headCircumference : Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )
    , ecd : Maybe ( WellChildECDId, WellChildECD )
    , albendazole : Maybe ( WellChildAlbendazoleId, WellChildAlbendazole )
    , mebendezole : Maybe ( WellChildMebendezoleId, WellChildMebendezole )
    , vitaminA : Maybe ( WellChildVitaminAId, WellChildVitaminA )
    , nextVisit : Maybe ( WellChildNextVisitId, WellChildNextVisit )
    , bcgImmunisation : Maybe ( WellChildBCGImmunisationId, WellChildBCGImmunisation )
    , dtpImmunisation : Maybe ( WellChildDTPImmunisationId, WellChildDTPImmunisation )
    , dtpStandaloneImmunisation : Maybe ( WellChildDTPStandaloneImmunisationId, WellChildDTPStandaloneImmunisation )
    , hpvImmunisation : Maybe ( WellChildHPVImmunisationId, WellChildHPVImmunisation )
    , ipvImmunisation : Maybe ( WellChildIPVImmunisationId, WellChildIPVImmunisation )
    , mrImmunisation : Maybe ( WellChildMRImmunisationId, WellChildMRImmunisation )
    , opvImmunisation : Maybe ( WellChildOPVImmunisationId, WellChildOPVImmunisation )
    , pcv13Immunisation : Maybe ( WellChildPCV13ImmunisationId, WellChildPCV13Immunisation )
    , rotarixImmunisation : Maybe ( WellChildRotarixImmunisationId, WellChildRotarixImmunisation )
    , ncda : Maybe ( WellChildNCDAId, WellChildNCDA )
    , feeding : Maybe ( WellChildFeedingId, WellChildFeeding )
    , hygiene : Maybe ( WellChildHygieneId, WellChildHygiene )
    , foodSecurity : Maybe ( WellChildFoodSecurityId, WellChildFoodSecurity )
    , caring : Maybe ( WellChildCaringId, WellChildCaring )
    }


{-| A set of NCD measurements that correspond to the same NCD encounter.
-}
type alias NCDMeasurements =
    { coMorbidities : Maybe ( NCDCoMorbiditiesId, NCDCoMorbidities )
    , coreExam : Maybe ( NCDCoreExamId, NCDCoreExam )
    , creatinineTest : Maybe ( NCDCreatinineTestId, NCDCreatinineTest )
    , dangerSigns : Maybe ( NCDDangerSignsId, NCDDangerSigns )
    , familyHistory : Maybe ( NCDFamilyHistoryId, NCDFamilyHistory )
    , familyPlanning : Maybe ( NCDFamilyPlanningId, NCDFamilyPlanning )
    , hba1cTest : Maybe ( NCDHbA1cTestId, NCDHbA1cTest )
    , healthEducation : Maybe ( NCDHealthEducationId, NCDHealthEducation )
    , hivTest : Maybe ( NCDHIVTestId, NCDHIVTest )
    , labsResults : Maybe ( NCDLabsResultsId, NCDLabsResults )
    , lipidPanelTest : Maybe ( NCDLipidPanelTestId, NCDLipidPanelTest )
    , liverFunctionTest : Maybe ( NCDLiverFunctionTestId, NCDLiverFunctionTest )
    , medicationDistribution : Maybe ( NCDMedicationDistributionId, NCDMedicationDistribution )
    , medicationHistory : Maybe ( NCDMedicationHistoryId, NCDMedicationHistory )
    , outsideCare : Maybe ( NCDOutsideCareId, NCDOutsideCare )
    , pregnancyTest : Maybe ( NCDPregnancyTestId, NCDPregnancyTest )
    , randomBloodSugarTest : Maybe ( NCDRandomBloodSugarTestId, NCDRandomBloodSugarTest )
    , referral : Maybe ( NCDReferralId, NCDReferral )
    , socialHistory : Maybe ( NCDSocialHistoryId, NCDSocialHistory )
    , symptomReview : Maybe ( NCDSymptomReviewId, NCDSymptomReview )
    , urineDipstickTest : Maybe ( NCDUrineDipstickTestId, NCDUrineDipstickTest )
    , vitals : Maybe ( NCDVitalsId, NCDVitals )
    }


type alias ChildScoreboardMeasurements =
    { ncda : Maybe ( ChildScoreboardNCDAId, ChildScoreboardNCDA )
    , bcgImmunisation : Maybe ( ChildScoreboardBCGImmunisationId, ChildScoreboardBCGImmunisation )
    , dtpImmunisation : Maybe ( ChildScoreboardDTPImmunisationId, ChildScoreboardDTPImmunisation )
    , dtpStandaloneImmunisation : Maybe ( ChildScoreboardDTPStandaloneImmunisationId, ChildScoreboardDTPStandaloneImmunisation )
    , ipvImmunisation : Maybe ( ChildScoreboardIPVImmunisationId, ChildScoreboardIPVImmunisation )
    , mrImmunisation : Maybe ( ChildScoreboardMRImmunisationId, ChildScoreboardMRImmunisation )
    , opvImmunisation : Maybe ( ChildScoreboardOPVImmunisationId, ChildScoreboardOPVImmunisation )
    , pcv13Immunisation : Maybe ( ChildScoreboardPCV13ImmunisationId, ChildScoreboardPCV13Immunisation )
    , rotarixImmunisation : Maybe ( ChildScoreboardRotarixImmunisationId, ChildScoreboardRotarixImmunisation )
    }


type alias TuberculosisMeasurements =
    { diagnostics : Maybe ( TuberculosisDiagnosticsId, TuberculosisDiagnostics )
    , dot : Maybe ( TuberculosisDOTId, TuberculosisDOT )
    , followUp : Maybe ( TuberculosisFollowUpId, TuberculosisFollowUp )
    , healthEducation : Maybe ( TuberculosisHealthEducationId, TuberculosisHealthEducation )
    , medication : Maybe ( TuberculosisMedicationId, TuberculosisMedication )
    , referral : Maybe ( TuberculosisReferralId, TuberculosisReferral )
    , symptomReview : Maybe ( TuberculosisSymptomReviewId, TuberculosisSymptomReview )
    , treatmentReview : Maybe ( TuberculosisTreatmentReviewId, TuberculosisTreatmentReview )
    }


type alias HIVMeasurements =
    { diagnostics : Maybe ( HIVDiagnosticsId, HIVDiagnostics )
    , followUp : Maybe ( HIVFollowUpId, HIVFollowUp )
    , healthEducation : Maybe ( HIVHealthEducationId, HIVHealthEducation )
    , medication : Maybe ( HIVMedicationId, HIVMedication )
    , referral : Maybe ( HIVReferralId, HIVReferral )
    , symptomReview : Maybe ( HIVSymptomReviewId, HIVSymptomReview )
    , treatmentReview : Maybe ( HIVTreatmentReviewId, HIVTreatmentReview )
    }


{-| A set of measurements that includes all required data for
Stock management data presentation.
-}
type alias StockManagementMeasurements =
    { childFbf : Dict ChildFbfId Fbf
    , motherFbf : Dict MotherFbfId Fbf
    , stockUpdate : Dict StockUpdateId StockUpdate
    }


{-| This is like `ChildMeasurementList`, except that it just covers one
of each kind of measurements (rather than a list of each kind).

So, this is the type you'd use for the measurements for a child for
a particular session.

-}
type alias ChildMeasurements =
    { height : Maybe ( HeightId, Height )
    , muac : Maybe ( MuacId, Muac )
    , nutrition : Maybe ( ChildNutritionId, ChildNutrition )
    , photo : Maybe ( PhotoId, Photo )
    , weight : Maybe ( WeightId, Weight )
    , counselingSession : Maybe ( CounselingSessionId, CounselingSession )
    , fbf : Maybe ( ChildFbfId, Fbf )
    , contributingFactors : Maybe ( ContributingFactorsId, ContributingFactors )
    , followUp : Maybe ( FollowUpId, FollowUp )
    , healthEducation : Maybe ( GroupHealthEducationId, GroupHealthEducation )
    , sendToHC : Maybe ( GroupSendToHCId, GroupSendToHC )
    , ncda : Maybe ( GroupNCDAId, GroupNCDA )
    }


emptyChildMeasurements : ChildMeasurements
emptyChildMeasurements =
    { height = Nothing
    , muac = Nothing
    , nutrition = Nothing
    , photo = Nothing
    , weight = Nothing
    , counselingSession = Nothing
    , fbf = Nothing
    , contributingFactors = Nothing
    , followUp = Nothing
    , healthEducation = Nothing
    , sendToHC = Nothing
    , ncda = Nothing
    }


{-| Like `ChildMeasurements`, but for mothers.

Note that for `consent`, we could have multiple consents in the same session.
So, it is a `List` (possibly empty) rather than a `Maybe`.

-}
type alias MotherMeasurements =
    { attendance : Maybe ( AttendanceId, Attendance )
    , familyPlanning : Maybe ( FamilyPlanningId, FamilyPlanning )
    , consent : Dict ParticipantConsentId ParticipantConsent
    , lactation : Maybe ( LactationId, Lactation )
    , fbf : Maybe ( MotherFbfId, Fbf )
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { attendance = Nothing
    , familyPlanning = Nothing
    , consent = Dict.empty
    , lactation = Nothing
    , fbf = Nothing
    }


type alias Measurements =
    { mothers : Dict PersonId MotherMeasurements
    , children : Dict PersonId ChildMeasurements
    }


emptyMeasurements : Measurements
emptyMeasurements =
    { mothers = Dict.empty
    , children = Dict.empty
    }


{-| This a convenience for functions which want to take values wrapped
in the given way.

  - `update` indicates whether we're currently saving the edits, or any error
    from the last save.

  - `previous` is the most recently value that is not part of this editing
    session ... that is, a previous value to compare to

  - `current` is the value we've saved.

-}
type alias MeasurementData data =
    { previous : data
    , current : data
    , update : WebData ()
    }


type alias PreviousValuesSet =
    { height : Maybe Float
    , muac : Maybe Float
    , weight : Maybe Float
    , headCircumference : Maybe Float
    }


type alias PreviousMeasurementsSet =
    { heights : List ( NominalDate, Float )
    , muacs : List ( NominalDate, Float )
    , weights : List ( NominalDate, Float )
    , headCircumferences : List ( NominalDate, Float )
    }
