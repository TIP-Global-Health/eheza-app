module Pages.Prenatal.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsCurrentMode
    , showEndEncounterDialog : Bool
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.SendViaWhatsAppDialog.Model.ReportComponentAntenatal)
    }


emptyModel : Model
emptyModel =
    { labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , components = Nothing
    }


type PrenatalTestReport
    = TestPerformed PrenatalTestResult
    | TestNotPerformedKnownAsPositive


type LabResultsMode
    = LabResultsCurrent LabResultsCurrentMode
    | LabResultsHistory LabResultsHistoryMode


type LabResultsCurrentMode
    = LabResultsCurrentMain
    | LabResultsCurrentDipstickShort
    | LabResultsCurrentDipstickLong


type LabResultsHistoryMode
    = LabResultsHistoryHIV (List ( NominalDate, Maybe PrenatalTestReport ))
    | LabResultsHistoryHIVPCR (List ( NominalDate, Maybe HIVPCRResult ))
    | LabResultsHistorySyphilis (List ( NominalDate, Maybe PrenatalTestResult ))
    | LabResultsHistoryHepatitisB (List ( NominalDate, Maybe PrenatalTestReport ))
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
    , DiagnosisHeartburn
    , DiagnosisHeartburnPersistent
    , DiagnosisDeepVeinThrombosis
    , DiagnosisPelvicPainIntense
    , DiagnosisPelvicPainContinued
    , DiagnosisDepressionNotLikely
    , DiagnosisDepressionPossible
    , DiagnosisDepressionHighlyPossible
    , DiagnosisDepressionProbable
    , DiagnosisSuicideRisk
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
    , DiagnosisHIVDetectableViralLoad
    , DiagnosisDiscordantPartnership
    , DiagnosisHepatitisB
    , DiagnosisMalaria
    , DiagnosisMalariaWithAnemia
    , DiagnosisMalariaWithSevereAnemia
    , DiagnosisUrinaryTractInfection
    , DiagnosisUrinaryTractInfectionContinued
    , DiagnosisPyelonephritis
    , DiagnosisCandidiasis
    , DiagnosisCandidiasisContinued
    , DiagnosisGonorrhea
    , DiagnosisGonorrheaContinued
    , DiagnosisTuberculosis
    , DiagnosisTrichomonasOrBacterialVaginosis
    , DiagnosisTrichomonasOrBacterialVaginosisContinued
    , DiagnosisOther
    ]


type Msg
    = CloseEncounter PrenatalEncounterId
    | SetActivePage Page
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
