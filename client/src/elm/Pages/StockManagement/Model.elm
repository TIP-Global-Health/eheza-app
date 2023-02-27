module Pages.StockManagement.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.StockUpdate.Model exposing (StockCorrectionReason, StockSupplier)
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
    , dateRecorded : Maybe NominalDate
    , supplier : Maybe StockSupplier
    , batchNumber : Maybe String
    , dateExpires : Maybe NominalDate
    , quantity : Maybe Int
    , notes : Maybe String
    , dateRecordedSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , dateExpiresSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyReceiveStockForm : ReceiveStockForm
emptyReceiveStockForm =
    { confirmIdentity = Nothing
    , displayIdentityPopup = False
    , dateRecorded = Nothing
    , supplier = Nothing
    , batchNumber = Nothing
    , dateExpires = Nothing
    , quantity = Nothing
    , notes = Nothing
    , dateRecordedSelectorPopupState = Nothing
    , dateExpiresSelectorPopupState = Nothing
    }


type alias CorrectEntryForm =
    { confirmIdentity : Maybe Bool
    , displayIdentityPopup : Bool
    , date : Maybe Date
    , entryType : Maybe CorrectionEntryType
    , quantity : Maybe Int
    , reason : Maybe StockCorrectionReason
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyCorrectEntryForm : CorrectEntryForm
emptyCorrectEntryForm =
    { confirmIdentity = Nothing
    , displayIdentityPopup = False
    , date = Nothing
    , entryType = Nothing
    , quantity = Nothing
    , reason = Nothing
    , dateSelectorPopupState = Nothing
    }


type CorrectionEntryType
    = EntryAddition
    | EntrySubstraction


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
    | SetNotes String
    | HideReceiveStockIdentityPopup
    | SaveReceiveStock NurseId
      --  CorrectEntry form.
    | SetCorrectEntryConfirmIdentity Bool
    | SetDate Date
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetCorrectionEntryType String
    | SetQuantityDeducted String
    | SetCorrectionReason StockCorrectionReason
    | HideCorrectEntryIdentityPopup
    | SaveCorrectEntry
