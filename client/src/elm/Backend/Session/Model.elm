module Backend.Session.Model exposing (EditableSession, MsgEditableSession(..), OfflineSession, Session)

{-| A "session" refers to a group session with mothers and babies ... that is,
an occasion on which measurements are taken in a group setting.

The `Session` contains basic data for a session.

The `OfflineSession` and `EditableSession` are constructed (on the fly) from
more basic information we track in `Backend.Model`. Eventually, we could
consider getting rid of the middle-man, but it's convenient for the moment
because there was so much code written in terms of an `EditableSession`.

-}

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDateRange)
import Measurement.Model
import RemoteData exposing (WebData)


{-| This is the basic `Session` data ... essentially, for scheduling purposes.

The `training` field identifies a "training" session that is handled differently
if we're in the sandbox environment. It is intended to be a kind of session that
can be quickly created and destroyed. (As opposed to historical sessions which
are meant to stay throughout a training workshop).

-}
type alias Session =
    { scheduledDate : NominalDateRange
    , clinicId : ClinicId
    , closed : Bool
    , training : Bool
    }


{-| Originally, this represented the additional information we obtained when
downloading a session for "offline" use. Now, we populate this on the fly
from `Backend.Model`. Eventually, we could consider cutting out the middle-man,
but there is a lot of code which is written in terms of `OfflineSession` and
`EditableSession`.
-}
type alias OfflineSession =
    -- The particular session we're working on
    { session : Session

    -- Some configuration data.
    , allParticipantForms : EveryDictList ParticipantFormId ParticipantForm
    , everyCounselingSchedule : EveryCounselingSchedule

    -- We'll sort by mother's name. The children's sort order doesn't really
    -- mean anything, but it's easier to work with mothers and children as
    -- "participants" if we're using the same structure here for both.
    , mothers : EveryDictList MotherId Mother
    , children : EveryDictList ChildId Child

    -- These are all the measurements which have been saved. (Not necessarily
    -- synced to the backend yet).
    , historicalMeasurements : HistoricalMeasurements

    -- These are the measurements we're currently working on, that is, the ones
    -- for this very session, that have been saved (at least locally).
    , currentMeasurements : Measurements

    -- These represent the most recent measurement of each kind in
    -- `historicalMeasurements` that is not in `currentMeasurements`. That is,
    -- it is the most recent measurement we have before the current session, to
    -- be used to compare the current session with.
    , previousMeasurements : Measurements
    }


{-| This combines an OfflineSession with some structures that track
the set of forms we use to record measurements, and the requests
to save those measurements (locally).

`edits` includes things like attendance and whether we've closed the
session since downloading it.

The childForms and motherForms fields could be put elsewhere, since they aren't
backend concepts. However, they don't belong at the level of particular pages,
since we actually use them in two different pages and don't want multiple
sources of truth for what's going on in the editor. Plus, logically the
motherForms and childForms belong in the `EditableSession` since they are tied
to an editable session. And, we fundamentally want one for each child and
mother (to represent the edits for each child and mother) ... that way we
guarantee that they don't "leak" to the wrong child or mother.

-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , edits : MeasurementEdits
    , update : WebData ()
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
    | SetPhotoFileId Photo Int
