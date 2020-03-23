module Backend.Measurement.Model exposing (Attendance, ChildMeasurementList, ChildMeasurements, ChildNutrition, ChildNutritionSign(..), CounselingSession, DistributionNotice(..), FamilyPlanning, FamilyPlanningSign(..), Fbf, FbfForm, FbfValue, Height, HeightInCm(..), HistoricalMeasurements, Lactation, LactationForm, LactationSign(..), Measurement, MeasurementData, Measurements, MotherMeasurementList, MotherMeasurements, Muac, MuacInCm(..), MuacIndication(..), ParticipantConsent, ParticipantConsentValue, Photo, PhotoUrl(..), SavedMeasurement(..), Weight, WeightInKg(..), emptyChildMeasurementList, emptyChildMeasurements, emptyHistoricalMeasurements, emptyMeasurements, emptyMotherMeasurementList, emptyMotherMeasurements)

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

  - the type if the ID for the participant
  - the type of the value for this measurement

-}
type alias Measurement value =
    { dateMeasured : NominalDate
    , nurse : Maybe NurseId
    , participantId : PersonId
    , sessionId : Maybe SessionId
    , value : value
    }



-- SPECIFIC MEASUREMENT TYPES


{-| The string represents the URL of the photo -- that is, the URL which
we can reference in order to display the photo.
-}
type PhotoUrl
    = PhotoUrl String


type alias Photo =
    Measurement PhotoUrl


{-| For the various measurements that are floats, we wrap them in a type to
distinguish them, and make the units explicit.
-}
type MuacInCm
    = MuacInCm Float


type alias Muac =
    Measurement MuacInCm


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
    Measurement HeightInCm


type WeightInKg
    = WeightInKg Float


type alias Weight =
    Measurement WeightInKg


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
    Measurement (EverySet FamilyPlanningSign)


type LactationSign
    = Breastfeeding
    | NoLactationSigns


type alias Lactation =
    Measurement (EverySet LactationSign)


type alias LactationForm =
    { breastfeeding : Maybe Bool
    }


type DistributionNotice
    = DistributedFully
    | DistributedPartiallyLackOfStock
    | DistributedPartiallyOther


type alias Fbf =
    Measurement FbfValue


type alias FbfValue =
    { distributedAmount : Float
    , distributionNotice : DistributionNotice
    }


type alias FbfForm =
    { distributedFully : Maybe Bool
    , distributedAmount : Maybe Float
    , distributionNotice : Maybe DistributionNotice
    }


type alias ParticipantConsent =
    Measurement ParticipantConsentValue


type alias ParticipantConsentValue =
    { language : Language
    , formId : ParticipantFormId
    }


type alias Attendance =
    Measurement Bool


type ChildNutritionSign
    = AbdominalDistension
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | None
    | PoorAppetite


type alias ChildNutrition =
    Measurement (EverySet ChildNutritionSign)


type alias CounselingSession =
    Measurement ( CounselingTiming, EverySet CounselingTopicId )



-- UNIFIED MEASUREMENT TYPE


{-| A type which handles any kind of measurement along with its ID.
(Thus, it is a "saved" measurement that has been assigned an ID.)
-}
type SavedMeasurement
    = SavedAttendance AttendanceId Attendance
    | SavedFamilyPlanning FamilyPlanningId FamilyPlanning
    | SavedParticipantConsent ParticipantConsentId ParticipantConsent
    | SavedHeight HeightId Height
    | SavedMuac MuacId Muac
    | SavedChildNutrition ChildNutritionId ChildNutrition
    | SavedPhoto PhotoId Photo
    | SavedWeight WeightId Weight
    | SavedCounselingSession CounselingSessionId CounselingSession



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
