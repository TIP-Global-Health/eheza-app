module Participant.Model
    exposing
        ( AgeDay(..)
        , Participant
        , ParticipantId
        , ParticipantTypeFilter(..)
        , ParticipantType(..)
        , ParticipantsDict
        )

import Dict exposing (Dict)
import Backend.Child.Model exposing (Child)
import Backend.Mother.Model exposing (Mother)


type AgeDay
    = AgeDay Int


type alias ParticipantId =
    Int


type ParticipantType
    = ParticipantChild Child
    | ParticipantMother Mother


type alias Participant =
    { info : ParticipantType
    }


type alias ParticipantsDict =
    Dict ParticipantId Participant


type ParticipantTypeFilter
    = All
    | Children
    | Mothers
