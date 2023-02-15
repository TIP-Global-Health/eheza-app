module Pages.StockManagement.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


type Msg
    = SetActivePage Page
