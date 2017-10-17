module Backend.Measurement.Model exposing (..)

{-| This will eventually replace the `Measurement.Model`, but it's easier
to keep things compiling for the moment to do it here first. Or, it may
make sense to actually keep a `Data` hierarchy, to mark things that are
in the data layer.
-}

import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
import EverySet exposing (EverySet)
import Drupal.Restful exposing (Entity)
import Gizra.NominalDate exposing (NominalDate)


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
two types.  Second, we can use a record for each, to have multiple lists, each
of a specialized type (rather than a single list with a tagged type).
-}
type alias MotherMeasurements =
    { familyPlannings : List (Entity FamilyPlanningId FamilyPlanning)
    }


emptyMotherMeasurements : MotherMeasurements
emptyMotherMeasurements =
    { familyPlannings = []
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.

What type the inner types ought to be is an interesting question ... I'll start
simple with a `List` and see how that goes. For the moment, we're not actually
remembering the entity ID's here ... we don't need them (yet), because of the way in
which the process of fetching and storing these works at the moment.

- We fetch the measurements in a batch in an `OfflineSession`.

- We don't modify the `OfflineSession` itself ... instead, we remember any new or edited
  measurements separately.

- When we send the in a different hen we save updates & edits, we only
  allow one measurement per session on the backend, so we automatically do a create
  or update based on the session ID.

So, we don't need to track entity ID's here ... it's managed more simply without that,
given the way we upload in batches.
-}
type alias ChildMeasurements =
    { heights : List (Entity HeightId Height)
    , muacs : List (Entity MuacId Muac)
    , nutritions : List (Entity ChildNutritionId ChildNutrition)
    , photos : List (Entity PhotoId Photo)
    , weights : List (Entity WeightId Weight)
    }


emptyChildMeasurements : ChildMeasurements
emptyChildMeasurements =
    { heights = []
    , muacs = []
    , nutritions = []
    , photos = []
    , weights = []
    }


{-| This type just organizes historical measurements by mother or child, for
our convenience.
-}
type alias Measurements =
    { mothers : EveryDict MotherId MotherMeasurements
    , children : EveryDict ChildId ChildMeasurements
    }


emptyMeasurements : Measurements
emptyMeasurements =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
    }


{-| This represents a set of mother measurements that we're currently working on ...
that is, the measurements which may or may not have been made for the current session.
So, it's like `MotherMeasurements`, but this isn't all the historical measurements ...
just the ones for one particular session.

Note that we don't just organize the historical measurement list by session, since we're
not fundamentally interested in historical sessions ... once the session is over,
we're really just interested in the date of each previous measurement. It's only the
current session that's special -- otherwise, we can just organize the measurements
by date measured.
-}
type alias EditableMotherMeasurements =
    { familyPlanning : EditableEntity FamilyPlanningId FamilyPlanning
    }


emptyEditableMotherMeasurements : EditableMotherMeasurements
emptyEditableMotherMeasurements =
    { familyPlanning = NotFound
    }


{-| This represents the status of a measurement within a session. It functions
something like a `Maybe (Editable (Entity (StorageKey a) b))`, but collapsed in a single
type to make it easier to reason about, and to have more descriptive names
for the various possible states.

The relationship between this type and what is stored in `OfflineSession` deserves
a bit of thought. What we're tracking in `OfflineSession` is the measurements as
they are on the backend. You could (and in some cases should) actually use an
`Editable`-style type right within `OfflineSession` to track edits. However, in
our case, we upload edits in batches, rather than individually. Also, we only
allow editing one session at a time, not the whole range of historical data in
`OfflineSession`. So, it's convenient to let `OfflineSession` represent just what's
on the backend, rather than tracking editable things inside `OfflineSession`.

Now, the downside is that we're tracking the current session's measurements in two
places -- as historical data (along with other data) in `OfflineSession`, and as
an editable current session. So, that seemingly violates the "single source of
truth" ideal. However, it's not entirely straightforward, because the truth is
a bit complicated in this case -- there are a fair number of "truths" in play here:

- The historical measurements already stored on the backend (some of which may be
  for the current session).

- The edits currently stored in local storage.

- The edits the user is currently making on screen.

- The value which was shown to the user when they started editing.

These are all potentially significant, and all potentially require some logic
if they are updated aysnc while certain other changes are made. So, they are
all "truths" worth tracking -- the "single source of truth" ideal doesn't
demand that we collapse them together, just that we're clear about what
represents what. So, at least for the moment, the scheme is that:

- `OfflineSession` tracks the truth about what is actually on the backend

- A collection of `EditableEntities` tracks:
   - The backend value we originally showed the user for that measurement.
   - What the user has done.

This way, if we (eventually) push updates to the `OfflineSession` where
possible (i.e. make the offline session partally online, where possible),
we've got all the information we need to figure out what we need to show
to the user (in cases of conflicting edits, or whatever).

TODO: This type assumes that we're updating in batches ... there would be a
variant that would be useful in situations where we're updating individually.
I suppose that would mean adding something to account for the update request
in-flight ... i.e. `WebData` or the equivalent. We don't need that for
measurements because we'll update them in batches, so we don't need to track
individual requests.

So, the meaning of the tags is:

- NotFound

  We didn't have the entity on the backend, and we haven't made one locally
  yet.

- New

  We didn't have the entity on the backend, but we've made a new one locally.

- Unedited

  A value from the backend that hasn't been edited locally.

- Edited

  We showed the user this value from the backend, and they've now edited it.
  So, what we're tracking here is not actually the **current** value on the
  backend (that is tracked elsewhere), but instead what we showed the user when
  they started editing.

  The first `value` is what we showed the user from the backend, and the second
  `value` is

Eventually, one of the interesting functions is going to be something that
takes a newly-arrived backend value (e.g. from pusher) and makes the
appropriate state transition here. So, something like:

    backendValueArrived : key -> value -> EditableEntity key value -> EditableEntity key value
    backendValueArrived key -> value -> editable =
        case editable of
            NotFound ->
                -- We didn't have one, and now we do.
                Unedited key value

            New value ->
                -- We have a new value from the backend since we created one locally.
                -- So, we may need a new tag to display a possible conflict to the user.

            Unedited oldKey oldValue ->
                -- We had an un-edited value, so we can just update it.
                Unedited key value

            Edited key value value ->
                -- A new value has arrived while we are editing, so we'll need another
                -- tag to possibly display a conflict to the user.

            Deleted key value ->
                -- A new value has arrived from the backend after we deleted locally,
                -- but hadn't yet saved the delete. So, again, a possible conflict.

Or something like that ... that's not quite right, but it illustrates some of the jobs
we'd eventually want to do with a type like this.
-}
type EditableEntity key value
    = NotFound
    | New value
    | Unedited key value
    | Edited key value value
    | Deleted key value


type alias EditableChildMeasurements =
    { height : EditableEntity HeightId Height
    , muac : EditableEntity MuacId Muac
    , nutrition : EditableEntity ChildNutritionId ChildNutrition
    , photo : EditableEntity PhotoId Photo
    , weight : EditableEntity WeightId Weight
    }


emptyEditableChildMeasurements : EditableChildMeasurements
emptyEditableChildMeasurements =
    { height = NotFound
    , muac = NotFound
    , nutrition = NotFound
    , photo = NotFound
    , weight = NotFound
    }


{-| This tracks editable measurements for a whole batch of mothers and
children.
-}
type alias EditableMeasurements =
    { mothers : EveryDict MotherId EditableMotherMeasurements
    , children : EveryDict ChildId EditableChildMeasurements
    }


emptyEditableMeasurements : EditableMeasurements
emptyEditableMeasurements =
    { mothers = EveryDict.empty
    , children = EveryDict.empty
    }


{-| The string represents the URL of the photo.
-}
type PhotoValue
    = PhotoValue String


type alias Photo =
    Measurement ChildId PhotoValue


type MuacValue
    = MuacValue Float


type alias Muac =
    Measurement ChildId MuacValue


type HeightValue
    = HeightValue Float


type alias Height =
    Measurement ChildId HeightValue


type WeightValue
    = WeightValue Float


type alias Weight =
    Measurement ChildId WeightValue


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
