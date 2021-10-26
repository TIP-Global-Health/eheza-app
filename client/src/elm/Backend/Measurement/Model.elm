module Backend.Measurement.Model exposing (..)

{-| This module represents various measurements to be stored on the backend,
and cached in local storage.
-}

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
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



-- GROUP MEASUREMENT TYPES


{-| The string represents the URL of the photo -- that is, the URL which
we can reference in order to display the photo.
-}
type PhotoUrl
    = PhotoUrl String


type alias Photo =
    GroupMeasurement PhotoUrl


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
    GroupMeasurement FollowUpValue


type alias FollowUpValue =
    { options : EverySet FollowUpOption
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
    | OneMonths
    | TwoMonths
    | ThreeMonths



-- NUTRITION MEASUREMENTS


type alias NutritionMuac =
    NutritionMeasurement MuacInCm


type alias NutritionHeight =
    NutritionMeasurement HeightInCm


type alias NutritionNutrition =
    NutritionMeasurement NutritionValue


type alias NutritionPhoto =
    NutritionMeasurement PhotoUrl


type alias NutritionWeight =
    NutritionMeasurement WeightInKg


type alias NutritionSendToHC =
    NutritionMeasurement SendToHCValue


type alias NutritionHealthEducation =
    NutritionMeasurement HealthEducationValue


type alias NutritionContributingFactors =
    NutritionMeasurement (EverySet ContributingFactorsSign)


type alias NutritionFollowUp =
    NutritionMeasurement FollowUpValue



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



-- PRENATAL MEASUREMENTS


type alias BreastExamValue =
    { exam : EverySet BreastExamSign
    , selfGuidance : Bool
    }


type BreastExamSign
    = Mass
    | Discharge
    | Infection
    | NormalBreast


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
    , confident : Bool
    , confirmation : Bool
    }


type alias LastMenstrualPeriod =
    PrenatalMeasurement LastMenstrualPeriodValue


type MedicalHistorySign
    = UterineMyoma
    | Diabetes
    | CardiacDisease
    | RenalDisease
    | HypertensionBeforePregnancy
    | TuberculosisPast
    | TuberculosisPresent
    | Asthma
    | BowedLegs
    | HIV
    | MentalHealthHistory
    | NoMedicalHistorySigns


type alias MedicalHistory =
    PrenatalMeasurement (EverySet MedicalHistorySign)


type MedicationSign
    = IronAndFolicAcidSupplement
    | DewormingPill
    | NoMedication


type alias Medication =
    PrenatalMeasurement (EverySet MedicationSign)


type alias ObstetricalExamValue =
    { fundalHeight : HeightInCm
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
    , cSectionReason : EverySet CSectionReason
    , previousDelivery : EverySet PreviousDeliverySign
    , previousDeliveryPeriod : EverySet PreviousDeliveryPeriod
    , obstetricHistory : EverySet ObstetricHistorySign
    }


type CSectionReason
    = Breech
    | Emergency
    | FailureToProgress
    | None
    | Other


type PreviousDeliveryPeriod
    = LessThan18Month
    | MoreThan5Years
    | Neither


type PreviousDeliverySign
    = CSectionInPreviousDelivery
    | StillbornPreviousDelivery
    | BabyDiedOnDayOfBirthPreviousDelivery
    | PartialPlacentaPreviousDelivery
    | SevereHemorrhagingPreviousDelivery
    | ConvulsionsPreviousDelivery
    | ConvulsionsAndUnconsciousPreviousDelivery
    | NoPreviousDeliverySign


type ObstetricHistorySign
    = SuccessiveAbortions
    | SuccessivePrematureDeliveries
    | PreeclampsiaPreviousPregnancy
    | GestationalDiabetesPreviousPregnancy
    | IncompleteCervixPreviousPregnancy
    | RhNegative
    | NoObstetricHistorySign


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


type ResourceSign
    = MosquitoNet
    | NoResource


type alias PrenatalPhoto =
    PrenatalMeasurement PhotoUrl


type alias Resource =
    PrenatalMeasurement (EverySet ResourceSign)


type SocialHistorySign
    = AccompaniedByPartner
    | PartnerHivCounseling
    | NoSocialHistorySign


type SocialHistoryHivTestingResult
    = ResultHivPositive
    | ResultHivNegative
    | ResultHivIndeterminate
    | NoHivTesting


type alias SocialHistoryValue =
    { socialHistory : EverySet SocialHistorySign
    , hivTestingResult : SocialHistoryHivTestingResult
    }


type alias SocialHistory =
    PrenatalMeasurement SocialHistoryValue


type alias VitalsValue =
    { sys : Float
    , dia : Float
    , heartRate : Int
    , respiratoryRate : Int
    , bodyTemperature : Float
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
    PrenatalMeasurement (EverySet PrenatalHealthEducationSign)


type PrenatalHealthEducationSign
    = EducationExpectations
    | EducationVisitsReview
    | EducationWarningSigns
    | EducationHemorrhaging
    | EducationFamilyPlanning
    | EducationBreastfeeding
    | EducationImmunization
    | EducationHygiene
    | NoPrenatalHealthEducationSigns


type alias PrenatalFollowUp =
    PrenatalMeasurement PrenatalFollowUpValue


type alias PrenatalFollowUpValue =
    { options : EverySet FollowUpOption
    , assesment : PrenatalAssesment
    }


type PrenatalAssesment
    = AssesmentNormalPregnancy
    | AssesmentHighRiskPregnancy


type alias PrenatalSendToHC =
    PrenatalMeasurement SendToHCValue


type alias PrenatalAppointmentConfirmationValue =
    { date : NominalDate
    }


type alias PrenatalAppointmentConfirmation =
    PrenatalMeasurement PrenatalAppointmentConfirmationValue



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
    , reasonForNotSendingToHC : ReasonForNotSendingToHC
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
    | Coartem
    | ORS
    | Zinc
    | LemonJuiceOrHoney
    | Albendazole
    | Mebendezole
    | VitaminA
    | NoMedicationDistributionSigns


type AdministrationNote
    = NonAdministrationLackOfStock
    | NonAdministrationKnownAllergy
    | NonAdministrationPatientDeclined
    | NonAdministrationPatientUnableToAfford
    | NonAdministrationHomeBirth
    | NonAdministrationChildsCondition
    | NonAdministrationOther
    | AdministeredToday
    | AdministeredPreviously


type MedicationNonAdministrationSign
    = MedicationAmoxicillin AdministrationNote
    | MedicationCoartem AdministrationNote
    | MedicationORS AdministrationNote
    | MedicationZinc AdministrationNote
    | NoMedicationNonAdministrationSigns


type alias MedicationDistributionValue =
    { distributionSigns : EverySet MedicationDistributionSign
    , nonAdministrationSigns : EverySet MedicationNonAdministrationSign
    }


type alias MedicationDistribution =
    AcuteIllnessMeasurement MedicationDistributionValue


type alias AcuteIllnessMuac =
    AcuteIllnessMeasurement MuacInCm


type ReasonForNotSendingToHC
    = ClientRefused
    | NoAmbulance
    | ClientUnableToAffordFees
    | ReasonForNotSendingToHCOther
    | NoReasonForNotSendingToHC


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
    AcuteIllnessMeasurement (EverySet FollowUpOption)


type alias CovidTesting =
    AcuteIllnessMeasurement CovidTestingValue


type alias CovidTestingValue =
    { result : RapidTestResult
    , administrationNote : Maybe AdministrationNote
    }


type alias AcuteIllnessContactsTracing =
    AcuteIllnessMeasurement (List ContactTraceEntry)


type alias AcuteIllnessTraceContact =
    AcuteIllnessMeasurement ContactTraceEntry


type alias ContactTraceEntry =
    { personId : PersonId
    , firstName : String
    , secondName : String
    , phoneNumber : String
    , contactDate : NominalDate
    }



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
    WellChildMeasurement PhotoUrl


type alias WellChildWeight =
    WellChildMeasurement WeightInKg


type alias WellChildSendToHC =
    WellChildMeasurement SendToHCValue


type alias WellChildHealthEducation =
    WellChildMeasurement HealthEducationValue


type alias WellChildContributingFactors =
    WellChildMeasurement (EverySet ContributingFactorsSign)


type alias WellChildFollowUp =
    WellChildMeasurement FollowUpValue


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
    = VaccineBCG
    | VaccineOPV
    | VaccineDTP
    | VaccinePCV13
    | VaccineRotarix
    | VaccineIPV
    | VaccineMR
    | VaccineHPV


type VaccineDose
    = VaccineDoseFirst
    | VaccineDoseSecond
    | VaccineDoseThird
    | VaccineDoseFourth


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
    }


type DeliveryComplication
    = ComplicationGestationalDiabetes
    | ComplicationEmergencyCSection
    | ComplicationPreclampsia
    | ComplicationMaternalHemmorhage
    | ComplicationHiv
    | ComplicationMaternalDeath
    | ComplicationOther
    | NoDeliveryComplications


type alias WellChildNextVisit =
    WellChildMeasurement NextVisitValue


type alias NextVisitValue =
    { immunisationDate : Maybe NominalDate
    , pediatricVisitDate : Maybe NominalDate
    }


type alias WellChildBCGImmunisation =
    WellChildMeasurement VaccinationValue


type alias WellChildDTPImmunisation =
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
    , resource : Maybe ( ResourceId, Resource )
    , socialHistory : Maybe ( SocialHistoryId, SocialHistory )
    , vitals : Maybe ( VitalsId, Vitals )
    , prenatalPhoto : Maybe ( PrenatalPhotoId, PrenatalPhoto )
    , birthPlan : Maybe ( BirthPlanId, BirthPlan )
    , pregnancyTest : Maybe ( PregnancyTestId, PregnancyTest )
    , healthEducation : Maybe ( PrenatalHealthEducationId, PrenatalHealthEducation )
    , followUp : Maybe ( PrenatalFollowUpId, PrenatalFollowUp )
    , sendToHC : Maybe ( PrenatalSendToHcId, PrenatalSendToHC )
    , appointmentConfirmation : Maybe ( PrenatalAppointmentConfirmationId, PrenatalAppointmentConfirmation )
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
    , resource = Nothing
    , socialHistory = Nothing
    , vitals = Nothing
    , prenatalPhoto = Nothing
    , birthPlan = Nothing
    , pregnancyTest = Nothing
    , healthEducation = Nothing
    , followUp = Nothing
    , sendToHC = Nothing
    , appointmentConfirmation = Nothing
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
    , hpvImmunisation : Maybe ( WellChildHPVImmunisationId, WellChildHPVImmunisation )
    , ipvImmunisation : Maybe ( WellChildIPVImmunisationId, WellChildIPVImmunisation )
    , mrImmunisation : Maybe ( WellChildMRImmunisationId, WellChildMRImmunisation )
    , opvImmunisation : Maybe ( WellChildOPVImmunisationId, WellChildOPVImmunisation )
    , pcv13Immunisation : Maybe ( WellChildPCV13ImmunisationId, WellChildPCV13Immunisation )
    , rotarixImmunisation : Maybe ( WellChildRotarixImmunisationId, WellChildRotarixImmunisation )
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


intMeasurementNotSetValue : Int
intMeasurementNotSetValue =
    -999


floatMeasurementNotSetValue : Float
floatMeasurementNotSetValue =
    toFloat intMeasurementNotSetValue
