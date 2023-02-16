module Backend.StockUpdate.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PhotoUrl)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias StockUpdate =
    { nurse : NurseId
    , dateMeasured : NominalDate
    , updateType : StockUpdateType
    , quantity : Int
    , dateRecorded : Maybe NominalDate
    , dateExpires : Maybe NominalDate
    , batchNumber : Maybe String
    , supplier : Maybe StockSupplier
    , correctionReason : Maybe StockCorrectionReason

    -- @todo: Change to ImageUrl.
    , signature : PhotoUrl
    }


type StockUpdateType
    = UpdateReceivingSupplies
    | UpdateCorrection


type StockSupplier
    = SupplierMOH
    | SupplierRBC
    | SupplierUNICEF
    | SupplierRMSCentral
    | SupplierRMSDistrict
    | SupplierBUFMAR


type StockCorrectionReason
    = ReasonInputError
    | ReasonExpiration
    | ReasonMissing
    | ReasonOther


type alias Model =
    { requestState : WebData () }


emptyModel : Model
emptyModel =
    { requestState = NotAsked }


type Msg
    = CreateStockUpdate StockUpdate
    | HandleCreatedStockUpdate (WebData ( StockUpdateId, StockUpdate ))
