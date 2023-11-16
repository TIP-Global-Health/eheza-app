module Pages.Prenatal.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Components.ReportToWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (LabResultsMode)


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsMode
    , showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentAntenatal)
    }


emptyModel : Model
emptyModel =
    { labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    , components = Nothing
    }


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
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.ReportToWhatsAppDialog.Model.ReportComponentsList)
