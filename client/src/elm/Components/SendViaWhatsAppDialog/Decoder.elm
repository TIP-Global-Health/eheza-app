module Components.SendViaWhatsAppDialog.Decoder exposing (decodeReportType)

import Components.SendViaWhatsAppDialog.Model exposing (ReportType)
import Components.SendViaWhatsAppDialog.Utils exposing (reportTypeFromString)
import Json.Decode exposing (..)


decodeReportType : Decoder ReportType
decodeReportType =
    string
        |> andThen
            (\reportType ->
                reportTypeFromString reportType
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| reportType ++ " is not a recognized ReportType")
            )
