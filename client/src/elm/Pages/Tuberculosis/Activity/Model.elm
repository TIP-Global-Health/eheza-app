module Pages.Tuberculosis.Activity.Model exposing (DOTForm, DiagnosticsData, DiagnosticsForm, HealthEducationForm, MedicationData, MedicationTask(..), Model, Msg(..), NextStepsData, NextStepsTask(..), PrescribedMedicationForm, SymptomReviewData, SymptomReviewForm, TuberculosisFollowUpTestingStage(..), emptyDOTForm, emptyDiagnosticsData, emptyDiagnosticsForm, emptyHealthEducationForm, emptyMedicationData, emptyModel, emptyNextStepsData, emptyPrescribedMedicationForm, emptySymptomReviewData, emptySymptomReviewForm)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (FollowUpForm, OngoingTreatmentReviewForm, SendToHCForm, emptyFollowUpForm, emptyOngoingTreatmentReviewForm, emptySendToHCForm)
import Pages.Page exposing (Page)


type alias Model =
    { diagnosticsData : DiagnosticsData
    , medicationData : MedicationData
    , symptomReviewData : SymptomReviewData
    , nextStepsData : NextStepsData
    }


emptyModel : Model
emptyModel =
    { diagnosticsData = emptyDiagnosticsData
    , medicationData = emptyMedicationData
    , symptomReviewData = emptySymptomReviewData
    , nextStepsData = emptyNextStepsData
    }


type alias DiagnosticsData =
    { form : DiagnosticsForm
    , showEndEncounterDialog : Bool
    }


emptyDiagnosticsData : DiagnosticsData
emptyDiagnosticsData =
    { form = emptyDiagnosticsForm
    , showEndEncounterDialog = False
    }


type alias DiagnosticsForm =
    { diagnosed : Maybe Bool
    , isPulmonary : Maybe Bool
    , isPulmonaryDirty : Bool
    }


emptyDiagnosticsForm : DiagnosticsForm
emptyDiagnosticsForm =
    { diagnosed = Nothing
    , isPulmonary = Nothing
    , isPulmonaryDirty = False
    }


type alias MedicationData =
    { prescribedMedicationForm : PrescribedMedicationForm
    , dotForm : DOTForm
    , treatmentReviewForm : OngoingTreatmentReviewForm
    , activeTask : Maybe MedicationTask
    }


emptyMedicationData : MedicationData
emptyMedicationData =
    { prescribedMedicationForm = emptyPrescribedMedicationForm
    , dotForm = emptyDOTForm
    , treatmentReviewForm = emptyOngoingTreatmentReviewForm
    , activeTask = Nothing
    }


type alias PrescribedMedicationForm =
    { medications : Maybe (List TuberculosisPrescribedMedication)
    , medicationsNotChanged : Maybe Bool
    }


emptyPrescribedMedicationForm : PrescribedMedicationForm
emptyPrescribedMedicationForm =
    { medications = Nothing
    , medicationsNotChanged = Nothing
    }


type alias DOTForm =
    { provideToday : Maybe Bool
    , reasonNotProvidedToday : Maybe TuberculosisDOTSign
    , reasonNotProvidedTodayDirty : Bool
    , distributeMedications : Maybe Bool
    , reasonNotDistributedMedications : Maybe TuberculosisDOTSign
    , reasonNotDistributedMedicationsDirty : Bool
    }


emptyDOTForm : DOTForm
emptyDOTForm =
    { provideToday = Nothing
    , reasonNotProvidedToday = Nothing
    , reasonNotProvidedTodayDirty = False
    , distributeMedications = Nothing
    , reasonNotDistributedMedications = Nothing
    , reasonNotDistributedMedicationsDirty = False
    }


type MedicationTask
    = TaskPrescribedMedication
    | TaskDOT
    | TaskTreatmentReview


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    { nightSweats : Maybe Bool
    , bloodInSputum : Maybe Bool
    , weightLoss : Maybe Bool
    , severeFatigue : Maybe Bool
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    { nightSweats = Nothing
    , bloodInSputum = Nothing
    , weightLoss = Nothing
    , severeFatigue = Nothing
    }


type alias NextStepsData =
    { sendToHCForm : SendToHCForm
    , healthEducationForm : HealthEducationForm
    , followUpForm : FollowUpForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { sendToHCForm = emptySendToHCForm
    , healthEducationForm = emptyHealthEducationForm
    , followUpForm = emptyFollowUpForm
    , activeTask = Nothing
    }


type alias HealthEducationForm =
    { followUpTesting : Maybe Bool }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    { followUpTesting = Nothing }


type NextStepsTask
    = TaskReferral
    | TaskHealthEducation
    | TaskFollowUp


type TuberculosisFollowUpTestingStage
    = FollowUpTestingMonth1
    | FollowUpTestingMonth2
    | FollowUpTestingEndMonth2
    | FollowUpTestingEndMonth5
    | FollowUpTestingEndMonth6


type Msg
    = SetActivePage Page
      -- DIAGNOSTICS
    | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
    | SetEndEncounterDialogState Bool
    | SaveDiagnostics PersonId IndividualEncounterParticipantId (Maybe ( TuberculosisDiagnosticsId, TuberculosisDiagnostics ))
      -- MEDICATION
    | SetActiveMedicationTask MedicationTask
    | SetPrescribedMedicationsNotChanged Bool
    | SetPrescribedMedication TuberculosisPrescribedMedication
    | SavePrescribedMedication PersonId (Maybe ( TuberculosisMedicationId, TuberculosisMedication )) (Maybe MedicationTask)
    | SetDOTBoolInput (Bool -> DOTForm -> DOTForm) Bool
    | SetReasonNotProvidedToday TuberculosisDOTSign
    | SetReasonMedicationsNotDistributed TuberculosisDOTSign
    | SaveDOT PersonId (Maybe ( TuberculosisDOTId, TuberculosisDOT )) (Maybe MedicationTask)
    | SetTreatmentReviewBoolInput (Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) Bool
    | SetReasonForNotTaking ReasonForNotTaking
    | SetTotalMissedDoses String
    | SetAdverseEvent AdverseEvent
    | SaveTreatmentReview PersonId (Maybe ( TuberculosisTreatmentReviewId, TuberculosisTreatmentReview )) (Maybe MedicationTask)
      -- SYMPTOM REVIEW
    | SetSymptomReviewBoolInput (Bool -> SymptomReviewForm -> SymptomReviewForm) Bool
    | SaveSymptomReview PersonId (Maybe ( TuberculosisSymptomReviewId, TuberculosisSymptomReview ))
      -- NEXT STEPS
    | SetActiveNextStepsTask NextStepsTask
    | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducation PersonId (Maybe ( TuberculosisHealthEducationId, TuberculosisHealthEducation )) (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( TuberculosisFollowUpId, TuberculosisFollowUp )) (Maybe NextStepsTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNonReferral ReasonForNonReferral
    | SaveReferral PersonId (Maybe ( TuberculosisReferralId, TuberculosisReferral )) (Maybe NextStepsTask)
