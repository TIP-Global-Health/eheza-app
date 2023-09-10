module Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..), Model, Msg(..), emptyModel, medicalDiagnoses, obstetricalDiagnoses)

import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (LabResultsMode)


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsMode
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
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.SendViaWhatsAppDialog.Model.ReportComponentsList)
