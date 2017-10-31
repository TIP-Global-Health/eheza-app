module Participant.Model
    exposing
        ( AgeDay(..)
        , Participant(..)
        , ParticipantId(..)
        , ParticipantTypeFilter(..)
        )

{-| This module provides a type which is either a child or a mother.
Basically, it's sometimes useful to put children and mothers together
in lists etc., so this gives us a way to do that.
-}

import Backend.Entities exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Mother.Model exposing (Mother)


{-| TODO: Move this somewhere else.
-}
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
