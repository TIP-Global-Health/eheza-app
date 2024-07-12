module Pages.StockManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ImageUrl, StockCorrectionReason, StockSupplier)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { displayMode : DisplayMode
    , receiveStockForm : ReceiveStockForm
    , correctEntryForm : CorrectEntryForm

    -- This is used by month selector to determine
    -- the gap from current month. We allow to go back
    -- 12 months, so, valid values are between 0 and 11.
    , monthGap : Int
    }


emptyModel : Model
emptyModel =
    { displayMode = ModeMain
    , receiveStockForm = emptyReceiveStockForm
    , correctEntryForm = emptyCorrectEntryForm
    , monthGap = 0
    }


type alias AssembledData =
    Dict MonthYear DataForMonth


type alias DataForMonth =
    { startingStock : Maybe Float
    , received : Float
    , issued : Float
    , currentBalance : Maybe Float
    , consumptionAverage : Float
    }


maxMonthGap : Int
maxMonthGap =
    11


type alias MonthYear =
    ( Int, Int )


type DisplayMode
    = ModeMain
    | ModeMonthDetails Int
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
    , signature : Maybe ImageUrl
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
    , signature = Nothing
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
    , signature : Maybe ImageUrl
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
    , signature = Nothing
    , dateSelectorPopupState = Nothing
    }


type CorrectionEntryType
    = EntryAddition
    | EntrySubstraction


type Msg
    = SetActivePage Page
    | SetDisplayMode DisplayMode
    | StoreSignature
    | ClearSignaturePad
      -- Main menu.
    | ChangeMonthGap Int
      -- Month Details menu.
    | ChangeDetailsMonthGap Int
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
    | ReceiveStockHandleStoredSignature String
    | SaveReceiveStock NurseId
      --  CorrectEntry form.
    | SetCorrectEntryConfirmIdentity Bool
    | SetDate Date
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetCorrectionEntryType String
    | SetQuantityDeducted String
    | SetCorrectionReason StockCorrectionReason
    | HideCorrectEntryIdentityPopup
    | CorrectEntryHandleStoredSignature String
    | SaveCorrectEntry NurseId
