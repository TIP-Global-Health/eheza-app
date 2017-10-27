module Backend.Measurement.Model exposing (..)

{-| This module represents various measurements to be stored on the backend,
and cached in local storage.
-}

import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (Entity)


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


{-| The string represents the URL of the photo.
-}
type PhotoValue
    = PhotoValue String


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
    | Injection
    | Necklace
    | NoFamilyPlanning
    | Pill


type alias FamilyPlanning =
    Measurement MotherId (EverySet FamilyPlanningSign)


type ChildNutritionSign
    = AbdominalDisortion
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | None
    | PoorAppetite


type alias ChildNutrition =
    Measurement ChildId (EverySet ChildNutritionSign)



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
    { familyPlannings : List (Entity FamilyPlanningId FamilyPlanning)
    }


emptyMotherMeasurementList : MotherMeasurementList
emptyMotherMeasurementList =
    { familyPlannings = []
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes.

-}
type alias ChildMeasurementList =
    { heights : List (Entity HeightId Height)
    , muacs : List (Entity MuacId Muac)
    , nutritions : List (Entity ChildNutritionId ChildNutrition)
    , photos : List (Entity PhotoId Photo)
    , weights : List (Entity WeightId Weight)
    }


emptyChildMeasurementList : ChildMeasurementList
emptyChildMeasurementList =
    { heights = []
    , muacs = []
    , nutritions = []
    , photos = []
    , weights = []
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
of each kind of measurements (rather than a list of each kind). So, it
is the type you'd use if you wanted to apply a `List.head` to everything
in `ChildMeasurementList`, for instance.
-}
type alias ChildMeasurements =
    { height : Maybe (Entity HeightId Height)
    , muac : Maybe (Entity MuacId Muac)
    , nutrition : Maybe (Entity ChildNutritionId ChildNutrition)
    , photo : Maybe (Entity PhotoId Photo)
    , weight : Maybe (Entity WeightId Weight)
    }


emptyChildMeasurements : ChildMeasurements
emptyChildMeasurements =
    { height = Nothing
    , muac = Nothing
    , nutrition = Nothing
    , photo = Nothing
    , weight = Nothing
    }


{-| Like `ChildMeasurements`, but for mothers.
-}
type alias MotherMeasurements =
    { familyPlanning : Maybe (Entity FamilyPlanningId FamilyPlanning)
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { familyPlanning = Nothing
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
type Edit value
    = Unedited
      -- We've created a new measurement, which we didn't think was on the backend
      -- when the user created it. It has no key, since that will be supplied when
      -- we actually save it to the backend.
    | Created value
      -- We've edited the measurement. The `backend` value tracks what we thought
      -- was on the backend when the user performed the edit. (This may be valuable
      -- someday for conflict resolution). So, if we edit and the re-edit, the
      -- "backend" value remains the same ... until we update the backend.
    | Edited { backend : value, edited : value }
      -- We've deleted it ... that is, we now want to indicate that this measurement
      -- hasn't been taken at all. The value tracks what value we thought was on
      -- the backend when the user performed the delete (again, possibly valuable
      -- someday for conflict resolution)
    | Deleted value


{-| This represents a set of edits to Mother measurements.
-}
type alias MotherEdits =
    { familyPlanning : Edit FamilyPlanning
    }


emptyMotherEdits : MotherEdits
emptyMotherEdits =
    { familyPlanning = Unedited
    }


type alias ChildEdits =
    { height : Edit Height
    , muac : Edit Muac
    , nutrition : Edit ChildNutrition
    , photo : Edit Photo
    , weight : Edit Weight
    }


emptyChildEdits : ChildEdits
emptyChildEdits =
    { height = Unedited
    , muac = Unedited
    , nutrition = Unedited
    , photo = Unedited
    , weight = Unedited
    }


{-| This tracks editable measurements for a whole batch of mothers and
children.
-}
type alias MeasurementEdits =
    { mothers : EveryDict MotherId MotherEdits
    , children : EveryDict ChildId ChildEdits
    }


emptyMeasurementEdits : MeasurementEdits
emptyMeasurementEdits =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
    }


{-| This a convenience for functions which want to take values wrapped
in the given way.

The `status` indicates whether we're currently saving the edits, or any
error from the last save.

TODO: There is probably a slightly better way to do this, but this will
do for now.

-}
type alias MeasurementData data edits =
    { previous : data
    , current : data
    , edits : edits
    , status : WebData ()
    }
