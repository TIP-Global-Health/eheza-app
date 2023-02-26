module Backend.StockUpdate.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PhotoUrl)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias StockUpdate =
    { nurse : NurseId
    , dateMeasured : NominalDate
    , updateType : StockUpdateType
    , quantity : Int
    , dateRecorded : NominalDate
    , dateExpires : Maybe NominalDate
    , batchNumber : Maybe String
    , supplier : Maybe StockSupplier
    , notes : Maybe String
    , correctionReason : Maybe StockCorrectionReason
    , healthCenter : HealthCenterId
    , shard : Maybe HealthCenterId

    -- @todo: Change to ImageUrl.
    , signature : Maybe PhotoUrl
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
    { requestState : WebData ( StockUpdateId, StockUpdate ) }


emptyModel : Model
emptyModel =
    { requestState = NotAsked }


type Msg
    = CreateStockUpdate StockUpdate
    | HandleCreatedStockUpdate (WebData ( StockUpdateId, StockUpdate ))
