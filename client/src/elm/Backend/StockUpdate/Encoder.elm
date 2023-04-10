module Backend.StockUpdate.Encoder exposing (encodeStockUpdate)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (ImageUrl(..), StockCorrectionReason(..), StockSupplier(..), StockUpdate, StockUpdateType(..))
import Backend.StockUpdate.Model exposing (..)
import Backend.StockUpdate.Utils exposing (..)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


encodeStockUpdate : StockUpdate -> List ( String, Value )
encodeStockUpdate stockUpdate =
    let
        (ImageUrl url) =
            stockUpdate.signature
    in
    [ ( "nurse", encodeEntityUuid stockUpdate.nurse )
    , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD stockUpdate.dateMeasured )
    , ( "stock_update_type", encodeStockUpdateType stockUpdate.updateType )
    , ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD stockUpdate.dateRecorded )
    , ( "quantity", int stockUpdate.quantity )
    , ( "health_center", encodeEntityUuid stockUpdate.healthCenter )
    , ( "shard", encodeEntityUuid stockUpdate.healthCenter )
    , ( "signature", string url )
    , ( "deleted", bool False )
    , ( "type", string "stock_update" )
    ]
        ++ encodeIfExists "expiration_date" stockUpdate.dateExpires Gizra.NominalDate.encodeYYYYMMDD
        ++ encodeIfExists "batch_number" stockUpdate.batchNumber string
        ++ encodeIfExists "stock_supplier" stockUpdate.supplier encodeStockSupplier
        ++ encodeIfExists "notes" stockUpdate.notes string
        ++ encodeIfExists "stock_correction_reason" stockUpdate.correctionReason encodeStockCorrectionReason


encodeStockUpdateType : StockUpdateType -> Value
encodeStockUpdateType =
    stockUpdateTypeToString >> string


encodeStockSupplier : StockSupplier -> Value
encodeStockSupplier =
    stockSupplierToString >> string


encodeStockCorrectionReason : StockCorrectionReason -> Value
encodeStockCorrectionReason =
    stockCorrectionReasonToString >> string
