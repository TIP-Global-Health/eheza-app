module Participant.Utils exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType, MotherActivityType)
import Activity.Utils exposing (childHasPendingActivity, motherHasPendingActivity, motherHasAnyPendingActivity, childHasAnyPendingActivity, getAllChildActivities, getAllMotherActivities)
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Utils exposing (getMyMother)
import EveryDict exposing (EveryDict)
import EveryDictList
import Measurement.Model
import Pages.Activity.Utils exposing (viewChildMeasurements, viewMotherMeasurements)
import Participant.Model exposing (Participant)


childParticipant : Participant ChildId Child ChildActivityType Measurement.Model.MsgChild
childParticipant =
    { activities = getAllChildActivities
    , getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate
    , getMotherId = \childId session -> getMyMother childId session.offlineSession |> Maybe.map Tuple.first
    , getName = .name
    , getParticipants = \session -> session.offlineSession.children
    , hasPendingActivity = childHasPendingActivity
    , iconClass = "child"
    , showProgressReportTab = True
    , tagActivityType = ChildActivity
    , toChildId = Just
    , toMotherId = always Nothing
    , viewMeasurements = viewChildMeasurements
    }


motherParticipant : Participant MotherId Mother MotherActivityType Measurement.Model.MsgMother
motherParticipant =
    -- TODO: getParticipants is inefficient ... should make the children and
    -- mothers match, and either pre-sort in EveryDictList or sort each time in
    -- EveryDict
    { activities = getAllMotherActivities
    , getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate
    , getMotherId = \motherId session -> Just motherId
    , getName = .name
    , getParticipants = \session -> session.offlineSession.mothers |> EveryDictList.toList |> EveryDict.fromList
    , hasPendingActivity = motherHasPendingActivity
    , iconClass = "mother"
    , showProgressReportTab = False
    , tagActivityType = MotherActivity
    , toChildId = always Nothing
    , toMotherId = Just
    , viewMeasurements = \language date zscores -> viewMotherMeasurements language date
    }
