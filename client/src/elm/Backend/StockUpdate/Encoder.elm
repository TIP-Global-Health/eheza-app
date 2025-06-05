module Backend.StockUpdate.Encoder exposing (encodeStockUpdate)

import Backend.Measurement.Model exposing (ImageUrl(..), StockCorrectionReason, StockSupplier, StockUpdate, StockUpdateType)
import Backend.StockUpdate.Utils exposing (..)
import Gizra.NominalDate
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeNullable)


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
    , ( "deleted", bool stockUpdate.deleted )
    , ( "type", string "stock_update" )
    ]
        ++ encodeNullable "expiration_date" stockUpdate.dateExpires Gizra.NominalDate.encodeYYYYMMDD
        ++ encodeNullable "batch_number" stockUpdate.batchNumber string
        ++ encodeNullable "stock_supplier" stockUpdate.supplier encodeStockSupplier
        ++ encodeNullable "notes" stockUpdate.notes string
        ++ encodeNullable "stock_correction_reason" stockUpdate.correctionReason encodeStockCorrectionReason


encodeStockUpdateType : StockUpdateType -> Value
encodeStockUpdateType =
    stockUpdateTypeToString >> string


encodeStockSupplier : StockSupplier -> Value
encodeStockSupplier =
    stockSupplierToString >> string


encodeStockCorrectionReason : StockCorrectionReason -> Value
encodeStockCorrectionReason =
    stockCorrectionReasonToString >> string
