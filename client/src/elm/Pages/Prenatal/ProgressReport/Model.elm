module Pages.Prenatal.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { labResultsMode = Nothing
    , showEndEncounterDialog = False
    }


type LabResultsMode
    = LabResultsCurrent
    | LabResultsHistory LabResultsHistoryMode


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


obstetricalDiagnoses : List PrenatalDiagnosis
obstetricalDiagnoses =
    [ DiagnosisGestationalHypertensionImmediate
    , DiagnosisGestationalHypertensionAfterRecheck
    , DiagnosisModeratePreeclampsiaImmediate
    , DiagnosisModeratePreeclampsiaAfterRecheck
    , DiagnosisSeverePreeclampsiaImmediate
    , DiagnosisSeverePreeclampsiaAfterRecheck
    , DiagnosisEclampsia
    , DiagnosisMiscarriage
    , DiagnosisMolarPregnancy
    , DiagnosisPlacentaPrevia
    , DiagnosisPlacentalAbruption
    , DiagnosisUterineRupture
    , DiagnosisObstructedLabor
    , DiagnosisPostAbortionSepsis
    , DiagnosisEctopicPregnancy
    , DiagnosisPROM
    , DiagnosisPPROM
    , DiagnosisHyperemesisGravidum
    , DiagnosisMaternalComplications
    , DiagnosisInfection
    , DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    ]


medicalDiagnoses : List PrenatalDiagnosis
medicalDiagnoses =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisSyphilis
    , DiagnosisSyphilisWithComplications
    , DiagnosisNeurosyphilis
    , DiagnosisModerateAnemia
    , DiagnosisSevereAnemia
    , DiagnosisSevereAnemiaWithComplications
    , DiagnosisHIV
    , DiagnosisDiscordantPartnership
    , DiagnosisHepatitisB
    , DiagnosisMalaria
    , DiagnosisMalariaWithAnemia
    , DiagnosisMalariaWithSevereAnemia
    ]


type Msg
    = CloseEncounter PrenatalEncounterId
    | SetActivePage Page
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
