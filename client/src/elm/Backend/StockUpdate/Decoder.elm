module Backend.StockUpdate.Decoder exposing (decodeStockUpdate)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.StockUpdate.Model exposing (..)
import Backend.StockUpdate.Utils exposing (..)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt, decodeStringWithDefault)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeStockUpdate : Decoder StockUpdate
decodeStockUpdate =
    succeed StockUpdate
        |> required "nurse" decodeEntityUuid
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "stock_update_type" decodeStockUpdateType
        |> required "quantity" decodeInt
        |> optional "execution_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "expiration_date" (nullable Gizra.NominalDate.decodeYYYYMMDD) Nothing
        |> optional "batch_number" (nullable string) Nothing
        |> optional "stock_supplier" (nullable decodeStockSupplier) Nothing
        |> optional "stock_correction_reason" (nullable decodeStockCorrectionReason) Nothing
        |> required "signature" (map PhotoUrl (decodeStringWithDefault ""))


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
