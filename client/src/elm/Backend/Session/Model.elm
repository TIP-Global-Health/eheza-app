module Backend.Session.Model exposing (CheckedIn, EditableSession, ExpectedParticipants, Model, Msg(..), OfflineSession, Session, batchSize, emptyModel)

{-| A "session" refers to a group session with mothers and babies ... that is,
an occasion on which measurements are taken in a group setting.

The `Session` contains basic data for a session.

The `OfflineSession` and `EditableSession` are constructed (on the fly) from
more basic information we track in `Backend.Model`. Eventually, we could
consider getting rid of the middle-man, but it's convenient for the moment
because there was so much code written in terms of an `EditableSession`.

-}

import Activity.Model exposing (SummaryByActivity, SummaryByParticipant)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType)
import Backend.Counseling.Model exposing (EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Gizra.NominalDate exposing (NominalDate)
import LocalData exposing (LocalData)
import Measurement.Model
import RemoteData exposing (RemoteData(..), WebData)


{-| This is the basic `Session` data ... essentially, for scheduling purposes.

The `training` field identifies a "training" session that is handled differently
if we're in the sandbox environment. It is intended to be a kind of session that
can be quickly created and destroyed. (As opposed to historical sessions which
are meant to stay throughout a training workshop).

-}
type alias Session =
    { startDate : NominalDate
    , endDate : Maybe NominalDate
    , clinicId : ClinicId
    , clinicType : ClinicType
    , deleted : Bool
    }


{-| We index in several ways.

Ideally this would be an opaque type in a separate module, so we can enforce
invariants (e.g. that when we add something to one list, we modify the others
appropriately). However, we only create this in one place, so it would be
overkill for now.

-}
type alias ExpectedParticipants =
    { byId : Dict PmtctParticipantId PmtctParticipant
    , byChildId : Dict PersonId (List PmtctParticipant)
    , byMotherId : Dict PersonId (List PmtctParticipant)
    }


type alias CheckedIn =
    { mothers : Dict PersonId Person
    , children : Dict PersonId Person
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
    , allParticipantForms : Dict ParticipantFormId ParticipantForm
    , everyCounselingSchedule : EveryCounselingSchedule

    -- This reflects everyone who is expected at the session, given the
    -- session's date and group.
    , participants : ExpectedParticipants

    -- These reflect the `Person` record for each person included in
    -- `participants`.
    , mothers : Dict PersonId Person
    , children : Dict PersonId Person

    -- This is lazy because it requires some significant calculation, and we
    -- don't always need it.
    , measurements :
        LocalData
            -- These are all the measurements which have been saved. (Not necessarily
            -- synced to the backend yet).
            { historical : HistoricalMeasurements

            -- These are the measurements we're currently working on, that is, the ones
            -- for this very session, that have been saved (at least locally).
            , current : Measurements

            -- These represent the most recent measurement of each kind in
            -- `historicalMeasurements` that is not in `currentMeasurements`. That is,
            -- it is the most recent measurement we have before the current session, to
            -- be used to compare the current session with.
            , previous : Measurements
            }
    }


{-| This adds to an OfflineSession some structures to keep track of editing.
-}
type alias EditableSession =
    { offlineSession : OfflineSession
    , update : WebData ()
    , checkedIn : LocalData CheckedIn
    , summaryByParticipant : LocalData SummaryByParticipant
    , summaryByActivity : LocalData SummaryByActivity
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to perform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeSessionRequest : WebData ()
    , saveAttendanceRequest : Dict PersonId (WebData ())
    , saveCounselingSessionRequest : Dict PersonId (WebData ())
    , saveFamilyPlanningRequest : Dict PersonId (WebData ())
    , saveLactationRequest : Dict PersonId (WebData ())
    , saveFbfRequest : Dict PersonId (WebData ())
    , saveHeightRequest : Dict PersonId (WebData ())
    , saveMuacRequest : Dict PersonId (WebData ())
    , saveNutritionRequest : Dict PersonId (WebData ())
    , saveParticipantConsentRequest : Dict PersonId (WebData ())
    , savePhotoRequest : Dict PersonId (WebData ())
    , saveWeightRequest : Dict PersonId (WebData ())
    , saveContributingFactorsRequest : Dict PersonId (WebData ())
    , saveFollowUpRequest : Dict PersonId (WebData ())
    , saveHealthEducationRequest : Dict PersonId (WebData ())
    , saveSendToHCRequest : Dict PersonId (WebData ())
    , saveNCDARequest : Dict PersonId (WebData ())
    }


emptyModel : Model
emptyModel =
    { closeSessionRequest = NotAsked
    , saveAttendanceRequest = Dict.empty
    , saveCounselingSessionRequest = Dict.empty
    , saveFamilyPlanningRequest = Dict.empty
    , saveLactationRequest = Dict.empty
    , saveFbfRequest = Dict.empty
    , saveHeightRequest = Dict.empty
    , saveMuacRequest = Dict.empty
    , saveNutritionRequest = Dict.empty
    , saveParticipantConsentRequest = Dict.empty
    , savePhotoRequest = Dict.empty
    , saveWeightRequest = Dict.empty
    , saveContributingFactorsRequest = Dict.empty
    , saveFollowUpRequest = Dict.empty
    , saveHealthEducationRequest = Dict.empty
    , saveSendToHCRequest = Dict.empty
    , saveNCDARequest = Dict.empty
    }


type Msg
    = CloseSession
    | HandleClosedSession (WebData ())
    | MeasurementOutMsgChild PersonId Measurement.Model.OutMsgChild
    | MeasurementOutMsgMother PersonId Measurement.Model.OutMsgMother
    | HandleSaveAttendance PersonId (WebData ())
    | HandleSaveCounselingSession PersonId (WebData ())
    | HandleSaveFamilyPlanning PersonId (WebData ())
    | HandleSaveLactation PersonId (WebData ())
    | HandleSaveFbf PersonId (WebData ())
    | HandleSaveHeight PersonId (WebData ())
    | HandleSaveMuac PersonId (WebData ())
    | HandleSaveNutrition PersonId (WebData ())
    | HandleSaveParticipantConsent PersonId (WebData ())
    | HandleSavePhoto PersonId (WebData ())
    | HandleSaveWeight PersonId (WebData ())
    | HandleSaveContributingFactors PersonId (WebData ())
    | HandleSaveFollowUp PersonId (WebData ())
    | HandleSaveHealthEducation PersonId (WebData ())
    | HandleSaveSendToHC PersonId (WebData ())
    | HandleSaveNCDA PersonId (WebData ())


batchSize : Int
batchSize =
    1000
