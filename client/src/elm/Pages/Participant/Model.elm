module Pages.Participant.Model
    exposing
        ( Msg(..)
        , Model
        , Tab(..)
        , emptyModel
        , thumbnailDimensions
        )

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import FilePicker.Model
import Measurement.Model
import Pusher.Model exposing (PusherEventData)


type Msg
    = HandlePusherEventData PusherEventData
    | MsgFilePicker FilePicker.Model.Msg
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
    , filePicker : FilePicker.Model.Model
    }


emptyModel : Model
emptyModel =
    { measurements = Measurement.Model.emptyModel
    , selectedActivity = Nothing
    , selectedTab = Pending
    , filePicker = FilePicker.Model.emptyMode
    }


thumbnailDimensions =
    { width = 222
    , height = 222
    }
