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
import Html exposing (Html)
import Pages.Activity.Model
import Translate exposing (Language)
import ZScore.Model


{-| Implementations for child and mother are in Participant.Utils.

`viewMeasurments` wouldn't ideally be here ... instead, we'd have smaller
things here that it needs. But this is faster for the moment.

-}
type alias Participant id value activity msg =
    { activities : List activity
    , getAvatarUrl : value -> Maybe String
    , getBirthDate : value -> Maybe NominalDate
    , getMotherId : id -> EditableSession -> Maybe MotherId
    , getName : value -> String
    , getParticipants : EditableSession -> EveryDict id value
    , hasPendingActivity : id -> activity -> EditableSession -> Bool
    , iconClass : String
    , showProgressReportTab : Bool
    , tagActivityType : activity -> ActivityType
    , toChildId : id -> Maybe ChildId
    , toMotherId : id -> Maybe MotherId
    , viewMeasurements : Language -> NominalDate -> ZScore.Model.Model -> id -> activity -> EditableSession -> Html (Pages.Activity.Model.Msg id msg)
    }
