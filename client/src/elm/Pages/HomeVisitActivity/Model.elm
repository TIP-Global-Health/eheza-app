module Pages.HomeVisitActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}
