module Pages.Patient.Model
    exposing
        ( Msg(..)
        , ActivityOptions(..)
        )

import App.PageType exposing (Page(..))
import Measurement.Model
import Pusher.Model exposing (PusherEventData)


type ActivityOptions
    = Weight


type Msg
    = HandlePusherEventData PusherEventData
    | MsgMeasurement Measurement.Model.Msg
    | SetRedirectPage Page
