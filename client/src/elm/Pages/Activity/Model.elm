module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)
import FilePicker.Model
import Measurement.Model
import Participant.Model exposing (ParticipantId, Participant)


type alias Model =
    { measurements : Measurement.Model.Model
    , selectedActivity : ActivityType
    , selectedParticipant : Maybe ( ParticipantId, Participant )
    , selectedTab : Tab
    , filePicker : FilePicker.Model.Model
    }


type Msg
    = SetRedirectPage Page
    | MsgFilePicker FilePicker.Model.Msg
    | MsgMeasurement ( ParticipantId, Participant ) Measurement.Model.Msg
    | SetSelectedParticipant (Maybe ( ParticipantId, Participant ))
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { measurements = Measurement.Model.emptyModel
    , selectedActivity = Child Height
    , selectedParticipant = Nothing
    , selectedTab = Pending
    , filePicker = FilePicker.Model.emptyMode
    }


thumbnailDimensions =
    { width = 96
    , height = 96
    }
