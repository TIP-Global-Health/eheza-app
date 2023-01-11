module Components.SendViaWhatsAppDialog.Utils exposing (..)

import Components.SendViaWhatsAppDialog.Model exposing (..)


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportWellChild ->
            "well-child"

        ReportAntenatal ->
            "antenatal"

        ReportAcuteIllness ->
            "acute-illness"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "well-child" ->
            Just ReportWellChild

        "antenatal" ->
            Just ReportAntenatal

        "acute-illness" ->
            Just ReportAcuteIllness

        _ ->
            Nothing
