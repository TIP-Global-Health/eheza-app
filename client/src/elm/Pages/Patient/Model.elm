module Pages.Patient.Model
    exposing
        ( Msg(..)
        , Model
        , ActivityOptions(..)
        , emptyModel
        )

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import Measurement.Model
import Pusher.Model exposing (PusherEventData)


-- @todo: Remove this.


type ActivityOptions
    = Weight


type Msg
    = HandlePusherEventData PusherEventData
    | MsgMeasurement Measurement.Model.Msg
    | SetRedirectPage Page
    | SetSelectedActivity (Maybe ActivityType)


type alias Model =
    { measurements : Measurement.Model.Model
    , selectedActivity : Maybe ActivityType
    }


emptyModel : Model
emptyModel =
    { measurements = Measurement.Model.emptyModel
    , selectedActivity = Nothing
    }
