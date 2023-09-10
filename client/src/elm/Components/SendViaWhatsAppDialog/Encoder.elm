module Components.SendViaWhatsAppDialog.Encoder exposing (encodeReportType)

import Components.SendViaWhatsAppDialog.Model exposing (ReportType)
import Components.SendViaWhatsAppDialog.Utils exposing (reportTypeToString)
import Json.Encode exposing (Value, string)


encodeReportType : ReportType -> Value
encodeReportType =
    reportTypeToString >> string
