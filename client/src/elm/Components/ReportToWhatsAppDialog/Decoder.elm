module Components.ReportToWhatsAppDialog.Decoder exposing (..)

import Components.ReportToWhatsAppDialog.Model exposing (ReportType)
import Components.ReportToWhatsAppDialog.Utils exposing (reportTypeFromString)
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
