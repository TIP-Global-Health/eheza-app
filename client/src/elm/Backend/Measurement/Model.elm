module Backend.Measurement.Model exposing
    ( AbdomenCPESign(..)
    , AcuteIllnessMeasurement
    , AcuteIllnessMeasurements
    , Attendance
    , BreastExam
    , BreastExamSign(..)
    , BreastExamValue
    , CSectionReason(..)
    , CSectionScar(..)
    , ChildMeasurementList
    , ChildMeasurements
    , ChildNutrition
    , ChildNutritionSign(..)
    , CorePhysicalExam
    , CorePhysicalExamValue
    , CounselingSession
    , DangerSign(..)
    , DangerSigns
    , EyesCPESign(..)
    , FamilyPlanning
    , FamilyPlanningSign(..)
    , FetalPresentation(..)
    , GroupMeasurement
    , HairHeadCPESign(..)
    , HandsCPESign(..)
    , HeartCPESign(..)
    , Height
    , HeightInCm(..)
    , HistoricalMeasurements
    , LastMenstrualPeriod
    , LastMenstrualPeriodValue
    , LegsCPESign(..)
    , LungsCPESign(..)
    , Measurement
    , MeasurementData
    , Measurements
    , MedicalHistory
    , MedicalHistorySign(..)
    , Medication
    , MedicationSign(..)
    , MotherMeasurementList
    , MotherMeasurements
    , Muac
    , MuacInCm(..)
    , MuacIndication(..)
    , NeckCPESign(..)
    , NutritionMeasurement
    , NutritionMeasurements
    , NutritionNutrition
    , ObstetricHistory
    , ObstetricHistorySign(..)
    , ObstetricHistoryStep2
    , ObstetricHistoryStep2Value
    , ObstetricHistoryValue
    , ObstetricalExam
    , ObstetricalExamValue
    , ParticipantConsent
    , ParticipantConsentValue
    , Photo
    , PhotoUrl(..)
    , PrenatalFamilyPlanning
    , PrenatalMeasurement
    , PrenatalMeasurements
    , PrenatalNutrition
    , PrenatalNutritionValue
    , PrenatalPhoto
    , PreviousDeliveryPeriod(..)
    , PreviousDeliverySign(..)
    , Resource
    , ResourceSign(..)
    , SocialHistory
    , SocialHistoryHivTestingResult(..)
    , SocialHistorySign(..)
    , SocialHistoryValue
    , SymptomsGI
    , SymptomsGISign(..)
    , SymptomsGeneral
    , SymptomsGeneralSign(..)
    , SymptomsRespiratory
    , SymptomsRespiratorySign(..)
    , Vitals
    , VitalsValue
    , Weight
    , WeightInKg(..)
    , emptyChildMeasurementList
    , emptyChildMeasurements
    , emptyHistoricalMeasurements
    , emptyMeasurements
    , emptyMotherMeasurementList
    , emptyMotherMeasurements
    )

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


{-| An interpretation of a MUAC, according to the measurement
tool referenced at <https://github.com/Gizra/ihangane/issues/282>
-}
type MuacIndication
    = MuacGreen
    | MuacRed
    | MuacYellow


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
    GroupMeasurement (EverySet ChildNutritionSign)


type alias CounselingSession =
    GroupMeasurement ( CounselingTiming, EverySet CounselingTopicId )



-- NUTRITION MEASUREMENTS


type alias NutritionNutrition =
    NutritionMeasurement (EverySet ChildNutritionSign)



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
    PrenatalMeasurement (EverySet DangerSign)


type alias LastMenstrualPeriodValue =
    { date : NominalDate
    , confident : Bool
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



-- ACUTE ILLNESS MEASUREMENTS


type SymptomsGeneralSign
    = BodyAches
    | Chills
    | SymptomGeneralFever
    | Headache
    | NightSweats
    | NoSymptomsGeneral


type alias SymptomsGeneral =
    AcuteIllnessMeasurement (EverySet SymptomsGeneralSign)


type SymptomsRespiratorySign
    = BloodInSputum
    | Cough
    | NasalCongestion
    | ShortnessOfBreath
    | SoreThroat
    | NoSymptomsRespiratory


type alias SymptomsRespiratory =
    AcuteIllnessMeasurement (EverySet SymptomsRespiratorySign)


type SymptomsGISign
    = SymptomGIAbdominalPain
    | BloodyDiarrhea
    | Nausea
    | NonBloodyDiarrhea
    | Vomiting
    | NoSymptomsGI


type alias SymptomsGI =
    AcuteIllnessMeasurement (EverySet SymptomsGISign)



-- LISTS OF MEASUREMENTS


type alias MotherMeasurementList =
    { attendances : Dict AttendanceId Attendance
    , familyPlannings : Dict FamilyPlanningId FamilyPlanning
    , consents : Dict ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { attendances = Dict.empty
    , familyPlannings = Dict.empty
    , consents = Dict.empty
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
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = Dict.empty
    , muacs = Dict.empty
    , nutritions = Dict.empty
    , photos = Dict.empty
    , weights = Dict.empty
    , counselingSessions = Dict.empty
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


{-| A set of prenatal measurements that correspond to the same prenatal
encounter.
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
    }


{-| A set of Nutrition measurements that correspond to the same Nutrition
encounter.
-}
type alias NutritionMeasurements =
    { nutrition : Maybe ( NutritionNutritionId, NutritionNutrition )
    }


{-| A set of Acute Illness measurements that correspond to the same Nutrition
encounter.
-}
type alias AcuteIllnessMeasurements =
    { symptomsGeneral : Maybe ( SymptomsGeneralId, SymptomsGeneral )
    , symptomsRespiratory : Maybe ( SymptomsRespiratoryId, SymptomsRespiratory )
    , symptomsGI : Maybe ( SymptomsGIId, SymptomsGI )
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
    }


emptyChildMeasurements : ChildMeasurements
emptyChildMeasurements =
    { height = Nothing
    , muac = Nothing
    , nutrition = Nothing
    , photo = Nothing
    , weight = Nothing
    , counselingSession = Nothing
    }


{-| Like `ChildMeasurements`, but for mothers.

Note that for `consent`, we could have multiple consents in the same session.
So, it is a `List` (possibly empty) rather than a `Maybe`.

-}
type alias MotherMeasurements =
    { attendance : Maybe ( AttendanceId, Attendance )
    , familyPlanning : Maybe ( FamilyPlanningId, FamilyPlanning )
    , consent : Dict ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { attendance = Nothing
    , familyPlanning = Nothing
    , consent = Dict.empty
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
