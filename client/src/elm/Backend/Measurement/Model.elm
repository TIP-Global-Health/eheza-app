module Backend.Measurement.Model exposing
    ( Attendance
    , ChildMeasurementList
    , ChildMeasurements
    , ChildNutrition
    , ChildNutritionSign(..)
    , CounselingSession
    , FamilyPlanning
    , FamilyPlanningSign(..)
    , GroupMeasurement
    , Height
    , HeightInCm(..)
    , HistoricalMeasurements
    , Measurement
    , MeasurementData
    , Measurements
    , MotherMeasurementList
    , MotherMeasurements
    , Muac
    , MuacInCm(..)
    , MuacIndication(..)
    , ParticipantConsent
    , ParticipantConsentValue
    , Photo
    , PhotoUrl(..)
    , PrenatalMeasurement
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
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Translate.Model exposing (Language)
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)



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
    , participantId : PersonId
    , encounterId : Maybe encounter
    , value : value
    }


type alias GroupMeasurement value =
    Measurement SessionId value


type alias PrenatalMeasurement value =
    Measurement PrenatalEncounterId value



-- SPECIFIC MEASUREMENT TYPES


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
    | None
    | PoorAppetite


type alias ChildNutrition =
    GroupMeasurement (EverySet ChildNutritionSign)


type alias CounselingSession =
    GroupMeasurement ( CounselingTiming, EverySet CounselingTopicId )



-- UNIFIED MEASUREMENT TYPE
-- LISTS OF MEASUREMENTS


type alias MotherMeasurementList =
    { attendances : EntityUuidDictList AttendanceId Attendance
    , familyPlannings : EntityUuidDictList FamilyPlanningId FamilyPlanning
    , consents : EntityUuidDictList ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { attendances = EntityUuidDictList.empty
    , familyPlannings = EntityUuidDictList.empty
    , consents = EntityUuidDictList.empty
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes.

-}
type alias ChildMeasurementList =
    { heights : EntityUuidDictList HeightId Height
    , muacs : EntityUuidDictList MuacId Muac
    , nutritions : EntityUuidDictList ChildNutritionId ChildNutrition
    , photos : EntityUuidDictList PhotoId Photo
    , weights : EntityUuidDictList WeightId Weight
    , counselingSessions : EntityUuidDictList CounselingSessionId CounselingSession
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = EntityUuidDictList.empty
    , muacs = EntityUuidDictList.empty
    , nutritions = EntityUuidDictList.empty
    , photos = EntityUuidDictList.empty
    , weights = EntityUuidDictList.empty
    , counselingSessions = EntityUuidDictList.empty
    }


{-| This type just organizes historical measurements by mother or child, for
our convenience.
-}
type alias HistoricalMeasurements =
    { mothers : EntityUuidDict PersonId MotherMeasurementList
    , children : EntityUuidDict PersonId ChildMeasurementList
    }


emptyHistoricalMeasurements : HistoricalMeasurements
emptyHistoricalMeasurements =
    { mothers = EntityUuidDict.empty
    , children = EntityUuidDict.empty
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
    , consent : EntityUuidDictList ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { attendance = Nothing
    , familyPlanning = Nothing
    , consent = EntityUuidDictList.empty
    }


type alias Measurements =
    { mothers : EntityUuidDict PersonId MotherMeasurements
    , children : EntityUuidDict PersonId ChildMeasurements
    }


emptyMeasurements : Measurements
emptyMeasurements =
    { mothers = EntityUuidDict.empty
    , children = EntityUuidDict.empty
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
