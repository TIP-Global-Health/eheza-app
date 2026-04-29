module Pages.StockManagement.Utils exposing (correctionEntryTypeFromString, correctionEntryTypeToString)

import Pages.StockManagement.Model exposing (CorrectionEntryType(..))


correctionEntryTypeToString : CorrectionEntryType -> String
correctionEntryTypeToString value =
    case value of
        EntryAddition ->
            "addition"

        EntrySubstraction ->
            "substraction"


correctionEntryTypeFromString : String -> Maybe CorrectionEntryType
correctionEntryTypeFromString value =
    case value of
        "addition" ->
            Just EntryAddition

        "substraction" ->
            Just EntrySubstraction

        _ ->
            Nothing
