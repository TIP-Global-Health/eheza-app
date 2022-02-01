module Pages.ClinicalProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
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
    | LabResultsHistoryProtein (List ( NominalDate, Maybe ProteinValue ))
    | LabResultsHistoryPH (List ( NominalDate, Maybe PHValue ))
    | LabResultsHistoryGlucose (List ( NominalDate, Maybe GlucoseValue ))
    | LabResultsHistoryLeukocytes (List ( NominalDate, Maybe LeukocytesValue ))
    | LabResultsHistoryNitrite (List ( NominalDate, Maybe NitriteValue ))
    | LabResultsHistoryUrobilinogen (List ( NominalDate, Maybe UrobilinogenValue ))
    | LabResultsHistoryHaemoglobin (List ( NominalDate, Maybe HaemoglobinValue ))
    | LabResultsHistorySpecificGravity (List ( NominalDate, Maybe SpecificGravityValue ))
    | LabResultsHistoryKetone (List ( NominalDate, Maybe KetoneValue ))
    | LabResultsHistoryBilirubin (List ( NominalDate, Maybe BilirubinValue ))
    | LabResultsHistoryRandomBloodSugar (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryHemoglobin (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryBloodGroup (List ( NominalDate, Maybe BloodGroup ))
    | LabResultsHistoryRhesus (List ( NominalDate, Maybe Rhesus ))


type Msg
    = SetActivePage Page
    | SetLabResultsHistoryMode (Maybe LabResultsHistoryMode)
