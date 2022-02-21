module Pages.PrenatalRecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import DateSelector.SelectorPopup exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (DropZoneFile, SendToHCForm, VitalsForm, emptySendToHCForm, emptyVitalsForm)
import Pages.Page exposing (Page)
import Pages.PrenatalActivity.Types exposing (..)


type Msg
    = NoOp
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe ( String, String ))


type alias Model =
    { showAlertsDialog : Bool

    -- Maybe (Danger Signs list, Instructions)
    , warningPopupState : Maybe ( String, String )
    }


emptyModel : Model
emptyModel =
    { showAlertsDialog = False
    , warningPopupState = Nothing
    }
