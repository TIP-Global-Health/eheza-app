module Pages.Participants.Model exposing (..)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityTypeList)
import App.PageType exposing (Page(..))
import Participant.Model exposing (ParticipantTypeFilter(..))
import Table


type alias Model =
    { activityTypeFilter : List ActivityType
    , participantTypeFilter : ParticipantTypeFilter
    , query : String
    , tableState : Table.State
    }


type Msg
    = SetActivityTypeFilter ActivityType Bool
    | SetActivityTypeFilters (List ActivityType)
    | SetParticipantTypeFilter String
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { activityTypeFilter = getActivityTypeList All
    , participantTypeFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
