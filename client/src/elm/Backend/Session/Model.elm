module Backend.Session.Model exposing (EditableSession, MsgEditableSession(..), OfflineSession, Session)

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

Note that `closed` here tracks what the backend thinks ... there is a separate
structure in `EditableSession.edits` that tracks whether we've closed a session
locally.

-}
type alias Session =
    { scheduledDate : NominalDateRange
    , clinicId : ClinicId
    , closed : Bool
    }


{-| This adds the additional information we get when we take a Session offline
for data-entry. It includes everything we need for data-entry. We get it from
/api/offline_sessions (and massage that a bit for convenience).
-}
type alias OfflineSession =
    -- The particular session we're working on
    { session : Session

    -- All the sessions for the relevant clinic, sorted by date. We need these
    -- for the progress report.
    , allSessions : EveryDictList SessionId Session

    -- We include the basic information about all the clinics so that we can at
    -- least present a limited UI on clinic pages that includes their name.
    , clinics : EveryDictList ClinicId Clinic

    -- We'll sort by mother's name
    , mothers : EveryDictList MotherId Mother
    , children : EveryDict ChildId Child

    -- These are all the measurements which have been saved to the backend.
    , historicalMeasurements : HistoricalMeasurements

    -- These are the measurements we're currently working on, that is,
    -- the ones for this very session, that have been saved to the backend.
    , currentMeasurements : Measurements

    -- These represent the most recent measurement of each kind in
    -- `historicalMeasurements` that is not in `currentMeasurements`. That is,
    -- it is the most recent measurement we have before the current session, to
    -- be used to compare the current session with.
    , previousMeasurements : Measurements
    }


{-| This combines an OfflineSession with a set of cached edits we're
currently working with for that Session, which haven't yet been saved to
the backend (but have been saved locally).

So, the `offlineSession` represents what's on the backend, while `edits`
represents what's cached in local storage, and not yet on the backend.

The `update` tracks whether we have a save in progress for the
cacehd edits. It's inside the type, because we can't save them unless
we have them ...

`edits` includes things like attendance and whether we've closed the
session since downloading it.

The childForms and motherForms fields could be put elsewhere, since they aren't
backend concepts. However, they don't belong at the level of particular pages,
since we actually use them in two different pages and don't want multiple
sources of truth for what's going on in the editor. Plus, logically the
motherForms and childForms belong in the `EditableSession` since they are tied
to an editable session. And, we fundamentally want one for each child and
mother (to represent the edits for each child and mother) ... that way we
guarantee that they don't "leak" to the wrong child or mother. So, it seems
that they belong in this type, but that may not be perfect. (This is the kind
of thing that `RestfulData` might eventually handle more elegantly).

-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , edits : MeasurementEdits
    , update : WebData ()
    , childForms : EveryDict ChildId Measurement.Model.ModelChild
    , motherForms : EveryDict MotherId Measurement.Model.ModelMother
    }


{-| This models some messages the UI can send to change an EditableSession.

They are actually handled in `Backend.Update`.

-}
type MsgEditableSession
    = CloseSession
    | MeasurementOutMsgChild ChildId Measurement.Model.OutMsgChild
    | MeasurementOutMsgMother MotherId Measurement.Model.OutMsgMother
    | RefetchSession
    | SetCheckedIn MotherId Bool
    | SetMotherForm MotherId Measurement.Model.ModelMother
    | SetChildForm ChildId Measurement.Model.ModelChild
    | SetPhotoFileId Photo Int
