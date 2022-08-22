module Pages.NCD.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}
