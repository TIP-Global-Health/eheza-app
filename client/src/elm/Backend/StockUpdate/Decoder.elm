module Backend.StockUpdate.Decoder exposing (decodeStockUpdate)

import Backend.Measurement.Model exposing (ImageUrl(..), StockCorrectionReason, StockSupplier, StockUpdate, StockUpdateType)
import Backend.StockUpdate.Utils exposing (..)
import Gizra.Json exposing (decodeInt, decodeStringWithDefault)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodeStockUpdate : Decoder StockUpdate
decodeStockUpdate =
    succeed StockUpdate
        |> required "nurse" decodeEntityUuid
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "stock_update_type" decodeStockUpdateType
        |> required "quantity" decodeInt
        |> required "execution_date" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "expiration_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "batch_number" (nullable string) Nothing
        |> optional "stock_supplier" (nullable decodeStockSupplier) Nothing
        |> optional "notes" (nullable string) Nothing
        |> optional "stock_correction_reason" (nullable decodeStockCorrectionReason) Nothing
        |> required "health_center" decodeEntityUuid
        |> required "deleted" (decodeWithFallback False bool)
        |> optional "shard" (nullable decodeEntityUuid) Nothing
        |> optional "signature" (map ImageUrl (decodeStringWithDefault "")) (ImageUrl "")


decodeStockUpdateType : Decoder StockUpdateType
decodeStockUpdateType =
    string
        |> andThen
            (\s ->
                stockUpdateTypeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized StockUpdateType."
                        )
            )


decodeStockSupplier : Decoder StockSupplier
decodeStockSupplier =
    string
        |> andThen
            (\s ->
                stockSupplierFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized StockSupplier."
                        )
            )


decodeStockCorrectionReason : Decoder StockCorrectionReason
decodeStockCorrectionReason =
    string
        |> andThen
            (\s ->
                stockCorrectionReasonFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized StockCorrectionReason."
                        )
            )
