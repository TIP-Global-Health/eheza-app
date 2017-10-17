module ParticipantManager.Model exposing (..)

import Dict exposing (Dict)
import Http
import Pages.Activities.Model
import Pages.Activity.Model
import Pages.Participant.Model
import Pages.Participants.Model
import Participant.Model exposing (Participant, ParticipantId)
import Pusher.Model exposing (PusherEvent)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { activitiesPage : Pages.Activities.Model.Model
    , activityPage : Pages.Activity.Model.Model
    , participantsPage : Pages.Participants.Model.Model
    , participantPage : Dict ParticipantId Pages.Participant.Model.Model
    }


type Msg
    = MsgPagesActivities Pages.Activities.Model.Msg
    | MsgPagesActivity Pages.Activity.Model.Msg
    | MsgPagesParticipant ParticipantId Pages.Participant.Model.Msg
    | MsgPagesParticipants Pages.Participants.Model.Msg


emptyModel : Model
emptyModel =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , activityPage = Pages.Activity.Model.emptyModel
    , participantsPage = Pages.Participants.Model.emptyModel
    , participantPage = Dict.empty
    }
