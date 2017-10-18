module Participant.Model
    exposing
        ( AgeDay(..)
        , Participant(..)
        , ParticipantId(..)
        , ParticipantTypeFilter(..)
        )

{-| This module provides a type which is either a child or a mother.
-}

import Backend.Entities exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Mother.Model exposing (Mother)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)


type AgeDay
    = AgeDay Int


type Participant
    = ParticipantChild Child
    | ParticipantMother Mother


type ParticipantId
    = ParticipantChildId ChildId
    | ParticipantMotherId MotherId


type ParticipantTypeFilter
    = All
    | Children
    | Mothers
