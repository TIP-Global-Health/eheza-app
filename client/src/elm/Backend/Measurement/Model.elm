module Backend.Measurement.Model exposing
    ( AbdomenCPESign(..)
    , Attendance
    , BreastExam
    , BreastExamSign(..)
    , BreastExamValue
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
    , ObstetricHistory
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
    , Resource
    , ResourceSign(..)
    , SocialHistory
    , SocialHistorySign(..)
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

import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
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
    = Condoms
    | IUD
    | Implant
    | Injection
    | Necklace
    | NoFamilyPlanning
    | Pill


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
    = AbnormalHeart
    | NormalHeart


type NeckCPESign
    = EnlargedThyroid
    | EnlargedLymphNodes
    | NormalNeck


type LungsCPESign
    = Wheezes
    | Crackles
    | NormalLungs


type AbdomenCPESign
    = Heptomegaly
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
    , cSectionScar : Bool
    }


type alias ObstetricalExam =
    PrenatalMeasurement ObstetricalExamValue


type FetalPresentation
    = Transverse
    | Breach
    | Cephalic


type alias ObstetricHistoryValue =
    { currentlyPregnant : Bool
    , termPregnancy : Int
    , pretermPregnancy : Int
    , stillBirthsAtTerm : Int
    , stillBirthsPreTerm : Int
    , abortions : Int
    , liveChildren : Int
    }


type alias ObstetricHistory =
    PrenatalMeasurement ObstetricHistoryValue


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


type alias Resource =
    PrenatalMeasurement (EverySet ResourceSign)


type SocialHistorySign
    = AccompaniedByPartner
    | PartnerHivCounseling
    | MentalHealthHistory
    | NoSocialHistorySign


type alias SocialHistory =
    PrenatalMeasurement (EverySet SocialHistorySign)


type alias VitalsValue =
    { sys : Float
    , dia : Float
    , heartRate : Int
    , respiratoryRate : Int
    , bodyTemperature : Float
    }


type alias Vitals =
    PrenatalMeasurement VitalsValue



-- LISTS OF MEASUREMENTS


type alias MotherMeasurementList =
    { attendances : EveryDictList AttendanceId Attendance
    , familyPlannings : EveryDictList FamilyPlanningId FamilyPlanning
    , consents : EveryDictList ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { attendances = EveryDictList.empty
    , familyPlannings = EveryDictList.empty
    , consents = EveryDictList.empty
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes.

-}
type alias ChildMeasurementList =
    { heights : EveryDictList HeightId Height
    , muacs : EveryDictList MuacId Muac
    , nutritions : EveryDictList ChildNutritionId ChildNutrition
    , photos : EveryDictList PhotoId Photo
    , weights : EveryDictList WeightId Weight
    , counselingSessions : EveryDictList CounselingSessionId CounselingSession
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = EveryDictList.empty
    , muacs = EveryDictList.empty
    , nutritions = EveryDictList.empty
    , photos = EveryDictList.empty
    , weights = EveryDictList.empty
    , counselingSessions = EveryDictList.empty
    }


{-| This type just organizes historical measurements by mother or child, for
our convenience.
-}
type alias HistoricalMeasurements =
    { mothers : EveryDict PersonId MotherMeasurementList
    , children : EveryDict PersonId ChildMeasurementList
    }


emptyHistoricalMeasurements : HistoricalMeasurements
emptyHistoricalMeasurements =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
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
    , familyPlanning : Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning )
    , nutrition : Maybe ( PrenatalNutritionId, PrenatalNutrition )
    , resource : Maybe ( ResourceId, Resource )
    , socialHistory : Maybe ( SocialHistoryId, SocialHistory )
    , vitals : Maybe ( VitalsId, Vitals )
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
    , consent : EveryDictList ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { attendance = Nothing
    , familyPlanning = Nothing
    , consent = EveryDictList.empty
    }


type alias Measurements =
    { mothers : EveryDict PersonId MotherMeasurements
    , children : EveryDict PersonId ChildMeasurements
    }


emptyMeasurements : Measurements
emptyMeasurements =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
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
