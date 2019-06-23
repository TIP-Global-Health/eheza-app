module Pages.Session.Model exposing (Model, Msg(..), emptyModel)

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Session.Model
import Measurement.Model
import Pages.Activities.Model
import Pages.Activity.Model
import Pages.Attendance.Model
import Pages.Page exposing (Page)
import Pages.Participant.Model
import Pages.Participants.Model


{-| This is where we track all the UI state that relates to an EditableSession
... that is, UI pages which will need an EditableSession to be supplied in
order to view or update them.
-}
type alias Model =
    -- Shows a list of activities ... user can select one.
    { activitiesPage : Pages.Activities.Model.Model

    -- Shows a list of mothers ... user can indicate attendance.
    , attendancePage : Pages.Attendance.Model.Model

    -- Shows a page for a single activity. We keep separate UI state for
    -- each activity.
    , childActivityPages : EveryDict ChildActivity (Pages.Activity.Model.Model PersonId)
    , motherActivityPages : EveryDict MotherActivity (Pages.Activity.Model.Model PersonId)

    -- Shows a list of participants ... user can select one.
    , participantsPage : Pages.Participants.Model.Model

    -- We keep separate page state per mother and child ... just models the
    -- selectedActivity and selectedTab, so it's not expensive. This means that
    -- we keep a separate selectedTab and selectedActivity per mother or child
    -- ... we could just keep a single state here if we wanted the selectedTab
    -- and selectedActivity to stay the same when you switch from one
    -- participant to another.
    , childPages : EntityUuidDict PersonId (Pages.Participant.Model.Model ChildActivity)
    , motherPages : EntityUuidDict PersonId (Pages.Participant.Model.Model MotherActivity)

    -- These forms appear on multiple pages, and we want to show the same state
    -- on each page. So, we keep them out here, and supply them as arguments.
    , childForms : EntityUuidDict PersonId Measurement.Model.ModelChild
    , motherForms : EntityUuidDict PersonId Measurement.Model.ModelMother
    }


emptyModel : Model
emptyModel =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , attendancePage = Pages.Attendance.Model.emptyModel
    , childActivityPages = Dict.empty
    , motherActivityPages = Dict.empty
    , childPages = Dict.empty
    , motherPages = Dict.empty
    , childForms = Dict.empty
    , motherForms = Dict.empty
    , participantsPage = Pages.Participants.Model.emptyModel
    }


{-| All the messages which relate to session pages.
-}
type Msg
    = MsgActivities Pages.Activities.Model.Msg
    | MsgAttendance Pages.Attendance.Model.Msg
    | MsgChildActivity ChildActivity (Maybe PersonId) (Pages.Activity.Model.Msg PersonId Measurement.Model.MsgChild)
    | MsgMotherActivity MotherActivity (Maybe PersonId) (Pages.Activity.Model.Msg PersonId Measurement.Model.MsgMother)
    | MsgChild PersonId (Pages.Participant.Model.Msg ChildActivity Measurement.Model.MsgChild)
    | MsgMother PersonId (Pages.Participant.Model.Msg MotherActivity Measurement.Model.MsgMother)
    | MsgParticipants Pages.Participants.Model.Msg
    | MsgSession Backend.Session.Model.Msg
    | SetActivePage Page
