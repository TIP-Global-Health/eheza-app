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
    { displayMode : DisplayMode
    , receiveStockForm : ReceiveStockForm
    }


emptyModel : Model
emptyModel =
    { displayMode = ModeMain
    , receiveStockForm = emptyReceiveStockForm
    }


type DisplayMode
    = ModeMain
    | ModeReceiveStock
    | ModeCorrectEntry


type StockManagementMenu
    = MenuReceiveStock
    | MenuViewMonthDetails
    | MenuCorrectEntry


type alias ReceiveStockForm =
    { confirmIdentity : Maybe Bool
    , displayIdentityPopup : Bool
    }


emptyReceiveStockForm : ReceiveStockForm
emptyReceiveStockForm =
    { confirmIdentity = Nothing
    , displayIdentityPopup = False
    }


type Msg
    = SetActivePage Page
    | SetDisplayMode DisplayMode
    | SetConfirmIdentity Bool
    | HideIdentityPopup
