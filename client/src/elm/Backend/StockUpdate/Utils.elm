module Backend.StockUpdate.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.StockUpdate.Model exposing (..)
import EverySet


stockUpdateTypeToString : StockUpdateType -> String
stockUpdateTypeToString value =
    case value of
        UpdateReceivingSupplies ->
            "receive-supply"

        UpdateCorrection ->
            "correction"


stockUpdateTypeFromString : String -> Maybe StockUpdateType
stockUpdateTypeFromString value =
    case value of
        "receive-supply" ->
            Just UpdateReceivingSupplies

        "correction" ->
            Just UpdateCorrection

        _ ->
            Nothing


stockSupplierToString : StockSupplier -> String
stockSupplierToString value =
    case value of
        SupplierMOH ->
            "moh"

        SupplierRBC ->
            "rbc"

        SupplierUNICEF ->
            "unicef"

        SupplierRMSCentral ->
            "rms-center"

        SupplierRMSDistrict ->
            "rms-district"

        SupplierBUFMAR ->
            "bufmar"


stockSupplierFromString : String -> Maybe StockSupplier
stockSupplierFromString value =
    case value of
        "moh" ->
            Just SupplierMOH

        "rbc" ->
            Just SupplierRBC

        "unicef" ->
            Just SupplierUNICEF

        "rms-center" ->
            Just SupplierRMSCentral

        "rms-district" ->
            Just SupplierRMSDistrict

        "bufmar" ->
            Just SupplierBUFMAR

        _ ->
            Nothing


stockCorrectionReasonToString : StockCorrectionReason -> String
stockCorrectionReasonToString value =
    case value of
        ReasonInputError ->
            "input-error"

        ReasonExpiration ->
            "expiration"

        ReasonMissing ->
            "missing"

        ReasonOther ->
            "other"


stockCorrectionReasonFromString : String -> Maybe StockCorrectionReason
stockCorrectionReasonFromString value =
    case value of
        "input-error" ->
            Just ReasonInputError

        "expiration" ->
            Just ReasonExpiration

        "missing" ->
            Just ReasonMissing

        "other" ->
            Just ReasonOther

        _ ->
            Nothing
