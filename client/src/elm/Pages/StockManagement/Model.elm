module Pages.StockManagement.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.StockUpdate.Model exposing (StockSupplier)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { displayMode : DisplayMode
    , receiveStockForm : ReceiveStockForm
    , correctEntryForm : CorrectEntryForm
    }


emptyModel : Model
emptyModel =
    { displayMode = ModeMain
    , receiveStockForm = emptyReceiveStockForm
    , correctEntryForm = emptyCorrectEntryForm
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
    , dateRecorded : Maybe Date
    , supplier : Maybe StockSupplier
    , batchNumber : String
    , dateExpires : Maybe Date
    , quantity : Maybe Int
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyReceiveStockForm : ReceiveStockForm
emptyReceiveStockForm =
    { confirmIdentity = Nothing
    , displayIdentityPopup = False
    , dateRecorded = Nothing
    , supplier = Nothing
    , batchNumber = ""
    , dateExpires = Nothing
    , quantity = Nothing
    , dateSelectorPopupState = Nothing
    }


type alias CorrectEntryForm =
    { confirmIdentity : Maybe Bool
    , displayIdentityPopup : Bool
    , date : Maybe Date
    , quantity : Maybe Int
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyCorrectEntryForm : CorrectEntryForm
emptyCorrectEntryForm =
    { confirmIdentity = Nothing
    , displayIdentityPopup = False
    , date = Nothing
    , quantity = Nothing
    , dateSelectorPopupState = Nothing
    }


type Msg
    = SetActivePage Page
    | SetDisplayMode DisplayMode
      --  ReceiveStock form.
    | SetReceiveStockConfirmIdentity Bool
    | SetDateRecorded Date
    | SetDateRecordedSelectorState (Maybe (DateSelectorConfig Msg))
    | SetStockSupplier String
    | SetBatchNumber String
    | SetDateExpires Date
    | SetDateExpiresSelectorState (Maybe (DateSelectorConfig Msg))
    | SetQuantityAdded String
    | HideReceiveStockIdentityPopup
    | SaveReceiveStock
      --  CorrectEntry form.
    | SetCorrectEntryConfirmIdentity Bool
    | SetDate Date
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetQuantityDeducted String
    | HideCorrectEntryIdentityPopup
    | SaveCorrectEntry
