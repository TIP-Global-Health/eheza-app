module Backend.Session.Model exposing (EditableSession, OfflineSession, Session)

{-| Represents an occasion on which measurements may be taken,
including the time and the place.
-}

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDateRange)


{-| This is the basic `Session` data that we get when we're
online, from /api/sessions.
-}
type alias Session =
    { scheduledDate : NominalDateRange
    , clinicId : ClinicId
    }


{-| This adds the additional information we get when we take
a Session offline for data-entry. It includes everything we need for
data-entry. We get it from /api/offline_sessions.
-}
type alias OfflineSession =
    { session : Session
    , clinic : Clinic

    -- We'll sort by mother's name
    , mothers : EveryDictList MotherId Mother
    , children : EveryDict ChildId Child
    , measurements : Measurements
    }


{-| This combines an OfflineSession with a set of EditableMeasurements we're
currently working with for that Session, which haven't yet been saved to
the backend.

Note that we'll need a function to construct this from what we get from the
backend, since the current session may already have some measurements in the
historical data. So, you can't necessarily just start with
`emptyEditableMeasurements`.

-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , edits : MeasurementEdits
    }
