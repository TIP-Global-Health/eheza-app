module Pages.Prenatal.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsCurrentMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
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
    , DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaInitialPhase
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhase
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
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
    , DiagnosisHyperemesisGravidumBySymptoms
    , DiagnosisSevereVomiting
    , DiagnosisSevereVomitingBySymptoms
    , DiagnosisMaternalComplications
    , DiagnosisInfection
    , DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    , DiagnosisHeartburn
    , DiagnosisHeartburnPersistent
    , DiagnosisDeepVeinThrombosis
    , DiagnosisPelvicPainIntense
    , DiagnosisPelvicPainContinued
    , DiagnosisGestationalDiabetes
    , DiagnosisRhesusNegative
    , DiagnosisDepressionNotLikely
    , DiagnosisDepressionPossible
    , DiagnosisDepressionHighlyPossible
    , DiagnosisDepressionProbable
    , DiagnosisSuicideRisk
    , DiagnosisPostpartumUrinaryIncontinence
    , DiagnosisPostpartumInfection
    , DiagnosisPostpartumExcessiveBleeding
    , DiagnosisPostpartumEarlyMastitisOrEngorgment
    , DiagnosisPostpartumMastitis
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
    , DiagnosisDiabetes
    , DiagnosisOther
    , DiagnosisPostpartumHeadache
    , DiagnosisPostpartumPerinealPainOrDischarge
    , DiagnosisPostpartumFatigue
    , DiagnosisPostpartumAbdominalPain
    , DiagnosisPostpartumFever
    ]


type CHWAction
    = ActionPregnancyDating
    | ActionLabs
    | ActionDangerSignsPresent
    | ActionReferredToHealthCenter
    | ActionAppointmentConfirmation
    | ActionHealthEducation
    | ActionBirthPlan


type Msg
    = CloseEncounter PrenatalEncounterId
    | SetActivePage Page
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
