module Backend.StockUpdate.Encoder exposing (encodeStockUpdate)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.StockUpdate.Model exposing (..)
import Backend.StockUpdate.Utils exposing (..)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeStockUpdate : StockUpdate -> List ( String, Value )
encodeStockUpdate stockUpdate =
    let
        (PhotoUrl url) =
            stockUpdate.signature

        recorded =
            Maybe.map
                (\dateRecorded ->
                    [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD dateRecorded ) ]
                )
                stockUpdate.dateRecorded
                |> Maybe.withDefault []

        expiration =
            Maybe.map
                (\dateExpires ->
                    [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD dateExpires ) ]
                )
                stockUpdate.dateExpires
                |> Maybe.withDefault []

        batch =
            Maybe.map (\batchNumber -> [ ( "batch_number", string batchNumber ) ]) stockUpdate.batchNumber
                |> Maybe.withDefault []

        supplier =
            Maybe.map
                (\stockSupplier ->
                    [ ( "stock_supplier", encodeStockSupplier stockSupplier ) ]
                )
                stockUpdate.supplier
                |> Maybe.withDefault []

        correction =
            Maybe.map
                (\correctionReason ->
                    [ ( "stock_correction_reason", encodeStockCorrectionReason correctionReason ) ]
                )
                stockUpdate.correctionReason
                |> Maybe.withDefault []
    in
    [ ( "nurse", encodeEntityUuid stockUpdate.nurse )
    , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD stockUpdate.dateMeasured )
    , ( "stock_update_type", encodeStockUpdateType stockUpdate.updateType )
    , ( "quantity", int stockUpdate.quantity )
    , ( "signature", string url )
    , ( "deleted", bool False )
    , ( "type", string "stock_update" )
    ]
        ++ recorded
        ++ expiration
        ++ batch
        ++ supplier
        ++ correction


encodeStockUpdateType : StockUpdateType -> Value
encodeStockUpdateType =
    stockUpdateTypeToString >> string


encodeStockSupplier : StockSupplier -> Value
encodeStockSupplier =
    stockSupplierToString >> string


encodeStockCorrectionReason : StockCorrectionReason -> Value
encodeStockCorrectionReason =
    stockCorrectionReasonToString >> string
