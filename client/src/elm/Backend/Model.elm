module Backend.Model exposing (..)

{-| The `Backend` hierarchy is for code that represents entities from the
backend. It is reponsible for fetching them, saving them, etc.

  - There shouldn't be any UI code here (except possibly some UI that
    is specifically related to fetching and saving -- we'll see).

  - There shouldn't be data here that purely relates to the local state of the
    app. If it isn't persisted to the backend, that state can go elsewhere.

The nice thing about this is that we can segregate local state (like whether
a dialog box is open etc.) from the state that persists to the backend.
That way, we can more easily have a single source of truth for the
backend data -- we're not tempted to duplicate it in various places
in the UI.

-}

import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (EditableMeasurements)
import Backend.Session.Model exposing (OfflineSession, Session)
import Gizra.NominalDate exposing (NominalDate)
import Drupal.Restful exposing (Entity, EntityDictList)
import RemoteData exposing (RemoteData(..), WebData)


{-| This model basically represents things we have locally which also belong
on the backend. So, conceptually it is a kind of a local cache of some of the
things on the backend.
-}
type alias Model =
    -- For now, we fetch all the clinics from the backend at once when we need
    -- any. So, the `WebData` tracks that request, and the `EntityDictList`
    -- tracks its result.
    { clinics : WebData (EntityDictList ClinicId Clinic)

    -- This tracks, if we have one, the OfflineSession which we're currently
    -- doing data entry for. This the only piece of data from the backend which
    -- we're guaranteed to be able to access while offline (because we cache
    -- it locally). So, it needs to contain everything we need for offline
    -- data entry.
    --
    -- The `WebData` wrapper represents whether we've tried to fetch it from
    -- our local cache (and any error that may have occurred). The inner
    -- `Maybe` represents whether it was actually found. That is, if we
    -- successfully query our local cache, and find it's not there, then the
    -- `WebData` layer is a `Success`, and the `Maybe` is a `Nothing`.
    --
    -- At least at first, we'll track our "mode" by whether we have an offline
    -- session in local storage. So:
    --
    -- * We'll automatically try to load an offline session from local storage
    --   when the app starts up.
    --
    -- * If we get one, we'll automatically show that in the UI, and prevent
    --   other things from showing.
    --
    -- * If we don't, then we'll show other things.
    --
    -- Note that this assumes that:
    --
    -- * We're only allowing a single offline session at a time to be stored
    --   locally.
    --
    -- * If we have one, we're definitely using it, not doing something else.
    , offlineSession : WebData (Maybe (Entity SessionId OfflineSession))

    -- This tracks mesaurements which we've edited, but haven't uploaded to
    -- the backend yet. These are immediately cached locally, which is what
    -- the `WebData` is for ... it wraps our effort to fetch from local
    -- strorage the measurements we've taken but not uploaded yet.
    --
    -- It's nice to track this separately, rather than integrating into
    -- the offlineSession with `EditableWebData`, because we upload these
    -- in a batch, all at once, rather than individually. So, we don't need
    -- to track the status of each measurement separately ... we deal with
    -- them together.
    --
    -- Note that we're assuming here that we'll save all of these before
    -- switching to another session ... that is, we can't have two sessions
    -- in progress at once. (We could change that down the road if necessary).
    --
    -- The inner `Maybe` represents whether we found any editable measurements
    -- in our local storage ... we'll delete the whole thing once we successfully
    -- save it to the backend.
    , editableMeasurements : WebData (Maybe EditableMeasurements)

    -- This tracks which sessions are currently available for data-entry,
    -- given the scheduled date range for each session. We remember which
    -- date we asked about, so that if the date changes (i.e. it becomes
    -- tomorrow, due to the passage of time), we can know that we ought to
    -- ask again.
    --
    -- TODO: Drupal.Restful should eventually have a `QueryResult` type which
    -- remembers the params we supplied and a WebData for the result ...
    -- since one would really always want to remember what query the results
    -- represent. (And, eventually, one would want to remember the `count`
    -- and which pages you have etc.).
    , openSessions : WebData ( NominalDate, EntityDictList SessionId Session )
    }


emptyModel : Model
emptyModel =
    { clinics = NotAsked
    , offlineSession = NotAsked
    , editableMeasurements = NotAsked
    , openSessions = NotAsked
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type Msg
    = FetchClinics
      -- For now, fetches the offline session from the backend ... will need to
      -- integrate caching, obviously!
    | FetchOfflineSession SessionId
    | FetchSessionsOpenOn NominalDate
    | HandleFetchedClinics (WebData (EntityDictList ClinicId Clinic))
    | HandleFetchedOfflineSession (WebData (Maybe (Entity SessionId OfflineSession)))
    | HandleFetchedSessions NominalDate (WebData (EntityDictList SessionId Session))
