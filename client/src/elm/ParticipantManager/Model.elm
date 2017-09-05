module ParticipantManager.Model exposing (..)

import Dict exposing (Dict)
import Http
import Pages.Activities.Model
import Pages.Participant.Model
import Pages.Participants.Model
import Participant.Model exposing (Participant, ParticipantId, ParticipantsDict)
import Pusher.Model exposing (PusherEvent)
import RemoteData exposing (RemoteData(..), WebData)


{-| We track any Participants we are currently subscribed to.

In theory, we'll only typically have one at a time. However, the logic of
subscribing and unsubscribing will be required in any event. Thus, it's
simpler to just track whatever we're subscribed to. That is, we could limit
ourselves to one subscription at a time, but that would actually be extra
logic, not less.

Each `Pages.Participant.Model.Model` is wrapped in a `WebData`, because we
derive it from fetching a `Participant` through `WebData` ... it's simplest to
just stay within the `WebData` container.

-}
type alias Model =
    { activitiesPage : Pages.Activities.Model.Model
    , participants : Dict ParticipantId (WebData Participant)
    , participantsPage : Pages.Participants.Model.Model
    , participantPage : Dict ParticipantId Pages.Participant.Model.Model
    }


{-| Our messages:

  - `Subscribe` means "fetch the Participant and listen to its pusher events"

  - `Unsubscribe` means "forget the Participant and stop listening to its pusher events"

  - `MsgPagesParticipant` is a message to route to a Participant viewer

-}
type Msg
    = Subscribe ParticipantId
    | Unsubscribe ParticipantId
    | FetchAll
    | MsgPagesActivities Pages.Activities.Model.Msg
    | MsgPagesParticipant ParticipantId Pages.Participant.Model.Msg
    | MsgPagesParticipants Pages.Participants.Model.Msg
    | HandleFetchedParticipant ParticipantId (Result Http.Error Participant)
    | HandleFetchedParticipants (Result Http.Error ParticipantsDict)
    | HandlePusherEvent (Result String PusherEvent)


emptyModel : Model
emptyModel =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , participants = Dict.empty
    , participantsPage = Pages.Participants.Model.emptyModel
    , participantPage = Dict.empty
    }
