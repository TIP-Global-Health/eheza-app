module Participant.Model exposing (Participant)

{-| This module provides a type which allows us to do certain things
with either children or mothers, by providing a typeclass-like
record which tells us how to do those things.

This also allows us to express a kind of association between
types ... e.g. MotherId goes with Mother and MotherActivity, while ChildId goes
with Child and ChildActivity.

-}

import Activity.Model exposing (ActivityType)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import EveryDict exposing (EveryDict)
import Gizra.NominalDate exposing (NominalDate)


{-| Implementations for child and mother are in Participant.Utils.
-}
type alias Participant id value activity =
    { activities : List activity
    , getAvatarThumb : value -> String
    , getBirthDate : value -> NominalDate
    , getMotherId : id -> EditableSession -> Maybe MotherId
    , getName : value -> String
    , getParticipants : EditableSession -> EveryDict id value
    , hasPendingActivity : id -> activity -> EditableSession -> Bool
    , iconClass : String
    , showProgressReportTab : Bool
    , tagActivityType : activity -> ActivityType
    , toChildId : id -> Maybe ChildId
    , toMotherId : id -> Maybe MotherId
    }
