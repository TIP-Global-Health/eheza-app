module Components.SendViaWhatsAppDialog.Encoder exposing (..)

import Components.SendViaWhatsAppDialog.Model exposing (ReportType)
import Components.SendViaWhatsAppDialog.Utils exposing (reportTypeToString)
import Json.Encode as Encoder exposing (Value, string)


encodeReportType : ReportType -> Value
encodeReportType =
    reportTypeToString >> string