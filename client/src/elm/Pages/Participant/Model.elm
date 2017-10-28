module Pages.Participant.Model
    exposing
        ( Msg(..)
        , Model
        , Tab(..)
        , emptyModel
        )

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import Backend.Entities exposing (..)
import FilePicker.Model
import Measurement.Model
import Pusher.Model exposing (PusherEventData)


type Msg
    = MsgMeasurementChild ChildId Measurement.Model.MsgChild
    | MsgMeasurementMother MotherId Measurement.Model.MsgMother
    | SetSelectedActivity (Maybe ActivityType)
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | ProgressReport


type alias Model =
    { selectedActivity : Maybe ActivityType
    , selectedTab : Tab
    }


emptyModel : Model
emptyModel =
    { selectedActivity = Nothing
    , selectedTab = Pending
    }
