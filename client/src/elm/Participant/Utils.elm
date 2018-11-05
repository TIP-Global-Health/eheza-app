module Participant.Utils exposing (childParticipant, motherParticipant)

import Activity.Model exposing (Activity(..), ChildActivity, MotherActivity)
import Activity.Utils exposing (summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Utils exposing (getMyMother)
import Measurement.Model
import Pages.Activity.Utils exposing (viewChildMeasurements, viewMotherMeasurements)
import Participant.Model exposing (Participant)


childParticipant : Participant ChildId Child ChildActivity Measurement.Model.MsgChild
childParticipant =
    { getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate >> Just
    , getMotherId = \childId session -> getMyMother childId session.offlineSession |> Maybe.map Tuple.first
    , getName = .name
    , getParticipants = \session -> session.offlineSession.children
    , iconClass = "child"
    , showProgressReportTab = True
    , summarizeActivitiesForParticipant = summarizeChildParticipant
    , summarizeParticipantsForActivity = summarizeChildActivity
    , tagActivity = ChildActivity
    , toChildId = Just
    , toMotherId = always Nothing
    , viewMeasurements = viewChildMeasurements
    }


motherParticipant : Participant MotherId Mother MotherActivity Measurement.Model.MsgMother
motherParticipant =
    { getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate
    , getMotherId = \motherId session -> Just motherId
    , getName = .name
    , getParticipants = \session -> session.offlineSession.mothers
    , iconClass = "mother"
    , showProgressReportTab = False
    , summarizeActivitiesForParticipant = summarizeMotherParticipant
    , summarizeParticipantsForActivity = summarizeMotherActivity
    , tagActivity = MotherActivity
    , toChildId = always Nothing
    , toMotherId = Just
    , viewMeasurements = \language date zscores -> viewMotherMeasurements language date
    }
