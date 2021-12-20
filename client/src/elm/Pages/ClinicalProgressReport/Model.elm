module Pages.ClinicalProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (GlucoseValue, PHValue, PrenatalTestResult, ProteinValue)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { labResultsHistoryMode : Maybe LabResultsHistoryMode
    }


emptyModel : Model
emptyModel =
    { labResultsHistoryMode = Nothing
    }


type LabResultsHistoryMode
    = LabResultsHistoryHIV (List ( NominalDate, Maybe PrenatalTestResult ))
    | LabResultsHistorySyphilis (List ( NominalDate, Maybe PrenatalTestResult ))
    | LabResultsHistoryHepatitisB (List ( NominalDate, Maybe PrenatalTestResult ))
    | LabResultsHistoryMalaria (List ( NominalDate, Maybe PrenatalTestResult ))
    | LabResultsHistoryUrineDipstick (List ( NominalDate, ( Maybe ProteinValue, Maybe PHValue, Maybe GlucoseValue ) ))
    | LabResultsHistoryRandomBloodSugar (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryHemoglobin (List ( NominalDate, Maybe Float ))


type Msg
    = SetActivePage Page
    | SetLabResultsHistoryMode (Maybe LabResultsHistoryMode)
