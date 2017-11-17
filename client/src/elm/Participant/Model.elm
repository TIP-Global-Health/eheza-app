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

We're actually using this type less and less. Instead, we're often using
function signatures that are parameterized in such a way as to be able to
handle children or mothers, or tracking children and mothers in separate
functions or lists. That allows us to express a kind of association between
types ... e.g. MotherId goes with Mother and MotherActivity, while ChildId goes
with Child and ChildActivity. But, sometimes referring to a `Participant` or
`ParticipantId` is still handy.

TODO: Move the things we still need from here elsewhere, or consolidate
the typeclass-like records here.

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
