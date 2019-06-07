module Participant.Model exposing (Participant, ParticipantId(..))

{-| This module provides a type which allows us to do certain things
with either children or mothers, by providing a typeclass-like
record which tells us how to do those things.

This also allows us to express a kind of association between
types ... e.g. MotherId goes with Mother and MotherActivity, while ChildId goes
with Child and ChildActivity.

-}

import Activity.Model exposing (Activity, CompletedAndPending)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html)
import Pages.Activity.Model
import Pages.Session.Model
import RemoteData exposing (WebData)
import Translate exposing (Language)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)
import ZScore.Model


{-| Implementations for child and mother are in Participant.Utils.

`viewMeasurments` wouldn't ideally be here ... instead, we'd have smaller
things here that it needs. But this is faster for the moment.

-}
type alias Participant id value activity msg =
    { getAvatarUrl : value -> Maybe String
    , getBirthDate : value -> Maybe NominalDate
    , getMotherId : id -> EditableSession -> Maybe PersonId
    , getName : value -> String
    , getParticipants : EditableSession -> EntityUuidDictList id value
    , getValue : id -> ModelIndexedDb -> WebData value
    , getVillage : value -> Maybe String
    , iconClass : String
    , showProgressReportTab : Bool
    , summarizeActivitiesForParticipant : id -> OfflineSession -> CompletedAndPending (List activity)
    , summarizeParticipantsForActivity : activity -> EditableSession -> CompletedAndPending (EntityUuidDictList id value)
    , tagActivity : activity -> Activity
    , toChildId : id -> Maybe PersonId
    , toMotherId : id -> Maybe PersonId
    , toParticipantId : id -> ParticipantId
    , viewMeasurements : Language -> NominalDate -> ZScore.Model.Model -> id -> activity -> Pages.Session.Model.Model -> EditableSession -> Html (Pages.Activity.Model.Msg id msg)
    }


type ParticipantId
    = ParticipantMother PersonId
    | ParticipantChild PersonId
