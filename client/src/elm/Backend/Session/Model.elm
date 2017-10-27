module Backend.Session.Model exposing (EditableSession, OfflineSession, Session)

{-| A "session" refers to an editing session ... that is, an occasion on
which measurements are taken.
-}

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDateRange)
import Measurement.Model
import RemoteData exposing (WebData)


{-| This is the basic `Session` data that we get when we're
online, from /api/sessions.
-}
type alias Session =
    { scheduledDate : NominalDateRange
    , clinicId : ClinicId
    }


{-| This adds the additional information we get when we take
a Session offline for data-entry. It includes everything we need for
data-entry. We get it from /api/offline_sessions (and massage that
a bit for convenience).
-}
type alias OfflineSession =
    { session : Session
    , clinic : Clinic

    -- We'll sort by mother's name
    , mothers : EveryDictList MotherId Mother
    , children : EveryDict ChildId Child

    -- These are all the measurements which are **not** part of the
    -- current session ... that is, all measurements which we're not
    -- editing at the moment. So, it inclues the "previousMeasurements"
    -- but not the "currentMeasurements".
    , historicalMeasurements : HistoricalMeasurements

    -- These are the measurements we're currently working on, that is,
    -- the ones for this very session, that have been saved to the backend.
    -- These are **not** included in `historicalMeasurements`
    , currentMeasurements : Measurements

    -- These represent the most recent measurement of each kind in
    -- `historicalMeasurements`. That is, it is the most recent measurement
    -- we have before the current session, to be used to compare the
    -- current session with. They are still included in `historicalMeasurements`
    -- as well, since we're not editing these ... they are stable.
    , previousMeasurements : Measurements
    }


{-| This combines an OfflineSession with a set of cached edits we're
currently working with for that Session, which haven't yet been saved to
the backend (but have been saved locally).

So, the `offlineSession` represents what's on the backend, while `edits`
represents what's cached in local storage, and not yet on the backend.

The `editStatus` tracks whether we have a save in progress for the
cacehd edits. It's inside the type, because we can't save them unless
we have them ...

TODO: The uiChild and uiMother fields may or may not best belong here, since
they aren't backend concepts. However, they don't belong at the level of
particular pages, since we actually use them in two different pages and
don't want multiple sources of truth for what's going on in the editor.
Plus, logically the uiChild and uiMother belong in the `EditableSession`
since they are tied to an editable session. And, we fundamentally want one
for each child and mother (to represent the edits for each child and
mother) ... that way we guarantee that they don't "leak" to the wrong child
or mother. So, it seems that they belong in this type, but that may not
be perfect. Possibly this whole type belongs somewhere else.

In fact, this is really a kind of **interface** type ... we don't actually
keep this type in the model as such.

-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , edits : MeasurementEdits
    , editStatus : WebData ()
    , uiChild : EveryDict ChildId Measurement.Model.ModelChild
    , uiMother : EveryDict MotherId Measurement.Model.ModelMother
    }
