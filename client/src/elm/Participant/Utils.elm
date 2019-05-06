module Participant.Utils exposing (childParticipant, motherParticipant)

import Activity.Model exposing (Activity(..), ChildActivity, MotherActivity)
import Activity.Utils exposing (summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Backend.Session.Utils exposing (getMyMother)
import EveryDict
import Measurement.Model
import Pages.Activity.Utils exposing (viewChildMeasurements, viewMotherMeasurements)
import Participant.Model exposing (Participant, ParticipantId(..))
import RemoteData exposing (RemoteData(..))


childParticipant : Participant PersonId Person ChildActivity Measurement.Model.MsgChild
childParticipant =
    { getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate
    , getMotherId = \childId session -> getMyMother childId session.offlineSession |> Maybe.map Tuple.first
    , getName = .name
    , getParticipants = \session -> session.offlineSession.children
    , getValue = \id db -> EveryDict.get id db.people |> Maybe.withDefault NotAsked
    , getVillage = .village
    , iconClass = "child"
    , showProgressReportTab = True
    , summarizeActivitiesForParticipant = summarizeChildParticipant
    , summarizeParticipantsForActivity = summarizeChildActivity
    , tagActivity = ChildActivity
    , toChildId = Just
    , toMotherId = always Nothing
    , toParticipantId = ParticipantChild
    , viewMeasurements = viewChildMeasurements
    }


motherParticipant : Participant PersonId Person MotherActivity Measurement.Model.MsgMother
motherParticipant =
    { getAvatarUrl = .avatarUrl
    , getBirthDate = .birthDate
    , getMotherId = \motherId session -> Just motherId
    , getName = .name
    , getParticipants = \session -> session.offlineSession.mothers
    , getValue = \id db -> EveryDict.get id db.people |> Maybe.withDefault NotAsked
    , getVillage = .village
    , iconClass = "mother"
    , showProgressReportTab = False
    , summarizeActivitiesForParticipant = summarizeMotherParticipant
    , summarizeParticipantsForActivity = summarizeMotherActivity
    , tagActivity = MotherActivity
    , toChildId = always Nothing
    , toMotherId = Just
    , toParticipantId = ParticipantMother
    , viewMeasurements = \language date zscores -> viewMotherMeasurements language date
    }
