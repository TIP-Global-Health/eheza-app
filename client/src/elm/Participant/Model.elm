module Participant.Model exposing (Participant)

{-| This module provides a type which allows us to do certain things
with either children or mothers, by providing a typeclass-like
record which tells us how to do those things.

This also allows us to express a kind of association between
types ... e.g. MotherId goes with Mother and MotherActivity, while ChildId goes
with Child and ChildActivity.

-}

import Activity.Model exposing (Activity, CompletedAndPending)
import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html)
import Pages.Activity.Model
import Pages.Session.Model
import RemoteData exposing (WebData)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language)
import ZScore.Model


{-| Implementations for child and mother are in Participant.Utils.

`viewMeasurments` wouldn't ideally be here ... instead, we'd have smaller
things here that it needs. But this is faster for the moment.

-}
type alias Participant id value activity msg date =
    { getAvatarUrl : value -> Maybe String
    , getBirthDate : value -> Maybe NominalDate
    , getMotherId : id -> EditableSession -> Maybe PersonId
    , getName : value -> String
    , getParticipants : EditableSession -> Dict id value
    , getValue : id -> ModelIndexedDb -> WebData value
    , getVillage : value -> Maybe String
    , iconClass : String
    , showProgressReportTab : Bool
    , summarizeActivitiesForParticipant :
        date
        -> ZScore.Model.Model
        -> EverySet SiteFeature
        -> id
        -> OfflineSession
        -> Bool
        -> ModelIndexedDb
        -> CompletedAndPending (List activity)
    , summarizeParticipantsForActivity :
        date
        -> ZScore.Model.Model
        -> EverySet SiteFeature
        -> activity
        -> OfflineSession
        -> Bool
        -> ModelIndexedDb
        -> CheckedIn
        -> CompletedAndPending (Dict id value)
    , tagActivity : activity -> Activity
    , toChildId : id -> Maybe PersonId
    , toMotherId : id -> Maybe PersonId
    , viewMeasurements :
        Language
        -> NominalDate
        -> Site
        -> ZScore.Model.Model
        -> Bool
        -> ModelIndexedDb
        -> id
        -> activity
        -> Pages.Session.Model.Model
        -> EditableSession
        -> Html (Pages.Activity.Model.Msg id msg)
    }
