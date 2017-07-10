module Pages.Patient.Model
    exposing
        ( Msg(..)
        , ActivityOptions(..)
        )

import App.PageType exposing (Page(..))
import Pusher.Model exposing (PusherEventData)


type ActivityOptions
    = Photo
    | Weight


type Msg
    = HandlePusherEventData PusherEventData
    | SetRedirectPage Page
