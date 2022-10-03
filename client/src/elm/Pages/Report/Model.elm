module Pages.Report.Model exposing (..)

import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias LabsResultsDisplayConfig =
    { bloodGpRs : Bool
    , hemoglobin : Bool
    , hepatitisB : Bool
    , hivPCR : Bool
    , malaria : Bool
    , syphilis : Bool
    , creatinine : Bool
    , liverFunction : Bool
    , pregnancy : Bool
    }


type alias LabsResultsValues encounterId =
    { hiv : List Backend.Measurement.Model.HIVTestValue
    , randomBloodSugar : List (Backend.Measurement.Model.RandomBloodSugarTestValue encounterId)
    , urineDipstick : List Backend.Measurement.Model.UrineDipstickTestValue
    , bloodGpRs : List (Backend.Measurement.Model.BloodGpRsTestValue encounterId)
    , hemoglobin : List Backend.Measurement.Model.HemoglobinTestValue
    , hepatitisB : List (Backend.Measurement.Model.HepatitisBTestValue encounterId)
    , hivPCR : List Backend.Measurement.Model.HIVPCRTestValue
    , malaria : List Backend.Measurement.Model.MalariaTestValue
    , syphilis : List (Backend.Measurement.Model.SyphilisTestValue encounterId)
    , creatinine : List Backend.Measurement.Model.CreatinineTestValue
    , liverFunction : List Backend.Measurement.Model.LiverFunctionTestValue
    , pregnancy : List Backend.Measurement.Model.PregnancyTestValue
    }


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
    | LabResultsHistoryCreatinine (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryBUN (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryALT (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryAST (List ( NominalDate, Maybe Float ))
    | LabResultsHistoryPregnancy (List ( NominalDate, Maybe TestReport ))


type TestReport
    = TestPerformed TestResult
    | TestNotPerformedKnownAsPositive


type DiagnosisMode
    = ModeActiveDiagnosis
    | ModeCompletedDiagnosis


type PaneEntryStatus
    = StatusOngoing
    | StatusResolved
