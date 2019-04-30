module Backend.Session.Model exposing (EditableSession, Model, Msg(..), OfflineSession, Session, emptyModel)

{-| A "session" refers to a group session with mothers and babies ... that is,
an occasion on which measurements are taken in a group setting.

The `Session` contains basic data for a session.

The `OfflineSession` and `EditableSession` are constructed (on the fly) from
more basic information we track in `Backend.Model`. Eventually, we could
consider getting rid of the middle-man, but it's convenient for the moment
because there was so much code written in terms of an `EditableSession`.

-}

import Backend.Counseling.Model exposing (EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDateRange)
import Measurement.Model
import RemoteData exposing (RemoteData(..), WebData)


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
    , mothers : EveryDictList PersonId Person
    , children : EveryDictList PersonId Person

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


{-| This adds to an OfflineSession some structures to keep track of editing.
-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , update : WebData ()
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeSessionRequest : WebData ()
    , saveAttendanceRequest : EveryDict PersonId (WebData ())
    , saveCounselingSessionRequest : EveryDict PersonId (WebData ())
    , saveFamilyPlanningRequest : EveryDict PersonId (WebData ())
    , saveHeightRequest : EveryDict PersonId (WebData ())
    , saveMuacRequest : EveryDict PersonId (WebData ())
    , saveNutritionRequest : EveryDict PersonId (WebData ())
    , saveParticipantConsentRequest : EveryDict PersonId (WebData ())
    , savePhotoRequest : EveryDict PersonId (WebData ())
    , saveWeightRequest : EveryDict PersonId (WebData ())
    }


emptyModel : Model
emptyModel =
    { closeSessionRequest = NotAsked
    , saveAttendanceRequest = EveryDict.empty
    , saveCounselingSessionRequest = EveryDict.empty
    , saveFamilyPlanningRequest = EveryDict.empty
    , saveHeightRequest = EveryDict.empty
    , saveMuacRequest = EveryDict.empty
    , saveNutritionRequest = EveryDict.empty
    , saveParticipantConsentRequest = EveryDict.empty
    , savePhotoRequest = EveryDict.empty
    , saveWeightRequest = EveryDict.empty
    }


type Msg
    = CloseSession
    | HandleClosedSession (WebData ())
    | MeasurementOutMsgChild PersonId Measurement.Model.OutMsgChild
    | MeasurementOutMsgMother PersonId Measurement.Model.OutMsgMother
    | HandleSaveAttendance PersonId (WebData ())
    | HandleSaveCounselingSession PersonId (WebData ())
    | HandleSaveFamilyPlanning PersonId (WebData ())
    | HandleSaveHeight PersonId (WebData ())
    | HandleSaveMuac PersonId (WebData ())
    | HandleSaveNutrition PersonId (WebData ())
    | HandleSaveParticipantConsent PersonId (WebData ())
    | HandleSavePhoto PersonId (WebData ())
    | HandleSaveWeight PersonId (WebData ())
