module Pages.Participant.Model
    exposing
        ( Msg(..)
        , Model
        , Tab(..)
        , emptyModel
        )

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import Measurement.Model
import Pusher.Model exposing (PusherEventData)


type Msg
    = HandlePusherEventData PusherEventData
    | MsgMeasurement Measurement.Model.Msg
    | SetRedirectPage Page
    | SetSelectedActivity (Maybe ActivityType)
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | ProgressReport


type alias Model =
    { measurements : Measurement.Model.Model
    , selectedActivity : Maybe ActivityType
    , selectedTab : Tab
    }


emptyModel : Model
emptyModel =
    { measurements = Measurement.Model.emptyModel
    , selectedActivity = Nothing
    , selectedTab = Pending
    }
