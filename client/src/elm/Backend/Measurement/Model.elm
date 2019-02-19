module Backend.Measurement.Model exposing (ChildMeasurementList, ChildMeasurements, ChildNutrition, ChildNutritionSign(..), CounselingSession, FamilyPlanning, FamilyPlanningSign(..), Height, HeightInCm(..), HistoricalMeasurements, Measurement, MeasurementData, Measurements, MotherMeasurementList, MotherMeasurements, Muac, MuacInCm(..), MuacIndication(..), ParticipantConsent, ParticipantConsentValue, Photo, PhotoValue, SavedMeasurement(..), Weight, WeightInKg(..), emptyChildMeasurementList, emptyChildMeasurements, emptyHistoricalMeasurements, emptyMeasurements, emptyMotherMeasurementList, emptyMotherMeasurements)

{-| This module represents various measurements to be stored on the backend,
and cached in local storage.
-}

import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
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
type alias Measurement participantId value =
    { participantId : participantId
    , sessionId : Maybe SessionId
    , dateMeasured : NominalDate
    , value : value
    }



-- SPECIFIC MEASUREMENT TYPES


{-| The string represents the URL of the photo -- that is, the URL which
we can reference in order to display the photo.

The `Maybe Int` represents the ID of the file entity on the backend, if
the file has been uploaded.

-}
type alias PhotoValue =
    { url : String
    , fid : Maybe Int
    }


type alias Photo =
    Measurement ChildId PhotoValue


{-| For the various measurements that are floats, we wrap them in a type to
distinguish them, and make the units explicit.
-}
type MuacInCm
    = MuacInCm Float


type alias Muac =
    Measurement ChildId MuacInCm


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
    Measurement ChildId HeightInCm


type WeightInKg
    = WeightInKg Float


type alias Weight =
    Measurement ChildId WeightInKg


type FamilyPlanningSign
    = Condoms
    | IUD
    | Implant
    | Injection
    | Necklace
    | NoFamilyPlanning
    | Pill


type alias FamilyPlanning =
    Measurement MotherId (EverySet FamilyPlanningSign)


type alias ParticipantConsent =
    Measurement MotherId ParticipantConsentValue


type alias ParticipantConsentValue =
    { language : Language
    , formId : ParticipantFormId
    }


type ChildNutritionSign
    = AbdominalDistension
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | None
    | PoorAppetite


type alias ChildNutrition =
    Measurement ChildId (EverySet ChildNutritionSign)


type alias CounselingSession =
    Measurement ChildId ( CounselingTiming, EverySet CounselingTopicId )



-- UNIFIED MEASUREMENT TYPE


{-| A type which handles any kind of measurement along with its ID.
(Thus, it is a "saved" measurement that has been assigned an ID.)
-}
type SavedMeasurement
    = SavedFamilyPlanning FamilyPlanningId FamilyPlanning
    | SavedParticipantConsent ParticipantConsentId ParticipantConsent
    | SavedHeight HeightId Height
    | SavedMuac MuacId Muac
    | SavedChildNutrition ChildNutritionId ChildNutrition
    | SavedPhoto PhotoId Photo
    | SavedWeight WeightId Weight
    | SavedCounselingSession CounselingSessionId CounselingSession



-- LISTS OF MEASUREMENTS


type alias MotherMeasurementList =
    { familyPlannings : List ( FamilyPlanningId, FamilyPlanning )
    , consents : List ( ParticipantConsentId, ParticipantConsent )
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { familyPlannings = []
    , consents = []
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes.

-}
type alias ChildMeasurementList =
    { heights : List ( HeightId, Height )
    , muacs : List ( MuacId, Muac )
    , nutritions : List ( ChildNutritionId, ChildNutrition )
    , photos : List ( PhotoId, Photo )
    , weights : List ( WeightId, Weight )
    , counselingSessions : List ( CounselingSessionId, CounselingSession )
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = []
    , muacs = []
    , nutritions = []
    , photos = []
    , weights = []
    , counselingSessions = []
    }


{-| This type just organizes historical measurements by mother or child, for
our convenience.
-}
type alias HistoricalMeasurements =
    { mothers : EveryDict MotherId MotherMeasurementList
    , children : EveryDict ChildId ChildMeasurementList
    }


emptyHistoricalMeasurements : HistoricalMeasurements
emptyHistoricalMeasurements =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
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
    { familyPlanning : Maybe ( FamilyPlanningId, FamilyPlanning )
    , consent : EveryDict ParticipantConsentId ParticipantConsent
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { familyPlanning = Nothing
    , consent = EveryDict.empty
    }


type alias Measurements =
    { mothers : EveryDict MotherId MotherMeasurements
    , children : EveryDict ChildId ChildMeasurements
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
