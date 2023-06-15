module Pages.ChildScoreboard.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.ChildScoreboard.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
