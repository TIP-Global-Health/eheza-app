module Backend.Measurement.Model exposing (ChildEdits, ChildMeasurementList, ChildMeasurements, ChildNutrition, ChildNutritionSign(..), CounselingSession, Edit(..), FamilyPlanning, FamilyPlanningSign(..), Height, HeightInCm(..), HistoricalMeasurements, Measurement, MeasurementData, MeasurementEdits, Measurements, MotherEdits, MotherMeasurementList, MotherMeasurements, Muac, MuacInCm(..), MuacIndication(..), ParticipantConsent, ParticipantConsentValue, Photo, PhotoValue, Weight, WeightInKg(..), emptyChildEdits, emptyChildMeasurementList, emptyChildMeasurements, emptyHistoricalMeasurements, emptyMeasurementEdits, emptyMeasurements, emptyMotherEdits, emptyMotherMeasurementList, emptyMotherMeasurements)

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
    { witness : NurseId
    , language : Language
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



-- LISTS OF MEASUREMENTS


{-| You'd be tempted to want a `List (Measurement a b)` in order to have a list
of measurements, but that won't work the way you might imagine, because the `a`
and the `b` would have to be the same for every element in the list. In other
words, you can't have heterogenous lists.

One way to deal with this would be to "tag" each element with a type
constructor that takes one specific type or another ... that is, something
like:

    type TaggedMeasurement
        = PhotoTag Photo
        | MuacTag Muac
        | HeightTag Height
        | WeightTag Weight
        | ChildNutritionTag ChildNutrition
        | FamklyPlanningTag FamilyPlanning

So, then you could have a `List TaggedMeasurment` ... in effect, the tags
function to "unify" the various types (and let us discriminate amongst them
at run-time).

We may eventually want something like that here, but for now we can do
something simpler. First, we'll generally know whether we're interested in a
set of child measurements or mother measurements, so we can specialize those
two types. Second, we can use a record for each, to have multiple lists, each
of a specialized type (rather than a single list with a tagged type).

-}
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



-- EDITING MEASUREMENTS


{-| As we edit measurements in our UI, we cache those edits in local storage,
so that eventually we can perform those edits on the backend. So, we define
a type which represents a possible edit.
-}
type Edit id value
    = Unedited
      -- We've created a new measurement, which we didn't think was on the backend
      -- when the user created it. It has no key, since that will be supplied when
      -- we actually save it to the backend.
    | Created value
      -- We've edited the measurement. The `backend` value tracks what we thought
      -- was on the backend when the user performed the edit. (This may be valuable
      -- someday for conflict resolution). So, if we edit and the re-edit, the
      -- "backend" value remains the same ... until we update the backend.
    | Edited
        { id : id
        , backend : value
        , edited : value
        }
      -- We've deleted it ... that is, we now want to indicate that this measurement
      -- hasn't been taken at all. The value tracks what value we thought was on
      -- the backend when the user performed the delete (again, possibly valuable
      -- someday for conflict resolution)
    | Deleted id value


{-| This represents a set of edits to Mother measurements.

`explicitlyCheckedIn` represents whether the mother has been **explicitly** marked as
being in attendance or not. (I had thought of just having no `MotherEdits` for
mothers who aren't in attendance, but that doesn't work so well, since we
construct default values where needed, so it's awkward to give the absence of
the value an implicit meaning).

But see the `isCheckedIn` function in `Activity.Utils`, which also checks whether
the mother is **implicitly** checked in, because she or a child has a completed
activity.

`consent` is a List, because we can have more than one consent in a session.
This is unlike, say, familyPlanning, where we'd only have one recorded for
each session.

-}
type alias MotherEdits =
    { familyPlanning : Edit FamilyPlanningId FamilyPlanning
    , consent : List (Edit ParticipantConsentId ParticipantConsent)
    , explicitlyCheckedIn : Bool
    }


emptyMotherEdits : MotherEdits
emptyMotherEdits =
    { familyPlanning = Unedited
    , consent = []
    , explicitlyCheckedIn = False
    }


type alias ChildEdits =
    { height : Edit HeightId Height
    , muac : Edit MuacId Muac
    , nutrition : Edit ChildNutritionId ChildNutrition
    , photo : Edit PhotoId Photo
    , weight : Edit WeightId Weight
    , counseling : Edit CounselingSessionId CounselingSession
    }


emptyChildEdits : ChildEdits
emptyChildEdits =
    { height = Unedited
    , muac = Unedited
    , nutrition = Unedited
    , photo = Unedited
    , weight = Unedited
    , counseling = Unedited
    }


{-| This tracks editable measurements for a whole batch of mothers and
children.

`explicitlyClosed` tracks whether the user has closed editing. (It could also
be closed because of the end date for the session, but that's tracked
elsewhere).

-}
type alias MeasurementEdits =
    { explicitlyClosed : Bool
    , mothers : EveryDict MotherId MotherEdits
    , children : EveryDict ChildId ChildEdits
    }


emptyMeasurementEdits : MeasurementEdits
emptyMeasurementEdits =
    { explicitlyClosed = False
    , mothers = EveryDict.empty
    , children = EveryDict.empty
    }


{-| This a convenience for functions which want to take values wrapped
in the given way.

  - `update` indicates whether we're currently saving the edits, or any error
    from the last save.

  - `previous` is the most recently value that is not part of this editing
    session ... that is, a previous value to compare to

  - `current` is the value stored **in the backend** for the current editing
    session (i.e. **not** a value cached locally that hasn't been saved yet).

  - `edits` represents changes which we want to make in this editing session. We
    save those changes locally and then apply them to the backend at some point.

There is possibly a slightly better way to do this ... in particular, it might
make sense to enforce some kind of relationship between `data` and `edits`.
But, that could make this less flexible ... this seems to do for now.

-}
type alias MeasurementData data edits =
    { previous : data
    , current : data
    , edits : edits
    , update : WebData ()
    }
