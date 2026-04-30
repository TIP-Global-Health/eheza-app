module Components.ReportToWhatsAppDialog.Encoder exposing (encodeReportType)

import Components.ReportToWhatsAppDialog.Model exposing (ReportType)
import Components.ReportToWhatsAppDialog.Utils exposing (reportTypeToString)
import Json.Encode exposing (Value, string)


encodeReportType : ReportType -> Value
encodeReportType =
    reportTypeToString >> string
