module Pages.Tuberculosis.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
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
    }


emptyDiagnosticsData : DiagnosticsData
emptyDiagnosticsData =
    { form = emptyDiagnosticsForm
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
    { medication : Maybe TuberculosisPrescribedMedication }


emptyPrescribedMedicationForm : PrescribedMedicationForm
emptyPrescribedMedicationForm =
    { medication = Nothing }


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


type Msg
    = SetActivePage Page
    | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
    | SaveDiagnostics PersonId (Maybe ( TuberculosisDiagnosticsId, TuberculosisDiagnostics ))
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
