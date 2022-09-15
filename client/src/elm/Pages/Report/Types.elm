module Pages.Report.Types exposing (..)

import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type LabResultsMode
    = LabResultsCurrent LabResultsCurrentMode
    | LabResultsHistory LabResultsHistoryMode


type LabResultsCurrentMode
    = LabResultsCurrentMain
    | LabResultsCurrentDipstickShort
    | LabResultsCurrentDipstickLong


type LabResultsHistoryMode
    = LabResultsHistoryHIV (List ( NominalDate, Maybe TestReport ))
    | LabResultsHistoryHIVPCR (List ( NominalDate, Maybe HIVPCRResult ))
    | LabResultsHistorySyphilis (List ( NominalDate, Maybe TestResult ))
    | LabResultsHistoryHepatitisB (List ( NominalDate, Maybe TestReport ))
    | LabResultsHistoryMalaria (List ( NominalDate, Maybe TestResult ))
    | LabResultsHistoryProtein (List ( NominalDate, Maybe ProteinValue ))
    | LabResultsHistoryPH (List ( NominalDate, Maybe PHValue ))
    | LabResultsHistoryGlucose (List ( NominalDate, Maybe GlucoseValue ))
    | LabResultsHistoryLeukocytes (List ( NominalDate, Maybe LeukocytesValue ))
    | LabResultsHistoryNitrite (List ( NominalDate, Maybe NitriteValue ))
    | LabResultsHistoryUrobilinogen (List ( NominalDate, Maybe UrobilinogenValue ))
    | LabResultsHistoryHaemoglobin (List ( NominalDate, Maybe HaemoglobinValue ))
    | LabResultsHistoryKetone (List ( NominalDate, Maybe KetoneValue ))
    | LabResultsHistoryBilirubin (List ( NominalDate, Maybe BilirubinValue ))
    | LabResultsHistoryRandomBloodSugar (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryHemoglobin (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryBloodGroup (List ( NominalDate, Maybe BloodGroup ))
    | LabResultsHistoryRhesus (List ( NominalDate, Maybe Rhesus ))


type TestReport
    = TestPerformed TestResult
    | TestNotPerformedKnownAsPositive
