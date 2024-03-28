module Pages.HIV.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( FollowUpForm
        , OngoingTreatmentReviewForm
        , SendToHCForm
        , emptyFollowUpForm
        , emptyOngoingTreatmentReviewForm
        , emptySendToHCForm
        )
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
    }


emptyDiagnosticsForm : DiagnosticsForm
emptyDiagnosticsForm =
    { diagnosed = Nothing
    }


type alias MedicationData =
    { prescribedMedicationForm : PrescribedMedicationForm
    , treatmentReviewForm : OngoingTreatmentReviewForm
    , activeTask : Maybe MedicationTask
    }


emptyMedicationData : MedicationData
emptyMedicationData =
    { prescribedMedicationForm = emptyPrescribedMedicationForm
    , treatmentReviewForm = emptyOngoingTreatmentReviewForm
    , activeTask = Nothing
    }


type alias PrescribedMedicationForm =
    { medications : Maybe (List HIVPrescribedMedication)
    , medicationsDirty : Bool
    }


emptyPrescribedMedicationForm : PrescribedMedicationForm
emptyPrescribedMedicationForm =
    { medications = Nothing
    , medicationsDirty = False
    }


type MedicationTask
    = TaskPrescribedMedication
    | TaskTreatmentReview


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    {}


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    {}


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
    {}


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    {}


type NextStepsTask
    = TaskReferral
    | TaskHealthEducation
    | TaskFollowUp


type Msg
    = SetActivePage Page



-- DIAGNOSTICS
-- | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
-- | SaveDiagnostics PersonId IndividualEncounterParticipantId (Maybe ( HIVDiagnosticsId, HIVDiagnostics ))
--   -- MEDICATION
-- | SetActiveMedicationTask MedicationTask
-- | SetPrescribedMedication HIVPrescribedMedication
-- | SavePrescribedMedication PersonId (Maybe ( HIVMedicationId, HIVMedication )) (Maybe MedicationTask)
-- | SetTreatmentReviewBoolInput (Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) Bool
-- | SetReasonForNotTaking ReasonForNotTaking
-- | SetTotalMissedDoses String
-- | SetAdverseEvent AdverseEvent
-- | SaveTreatmentReview PersonId (Maybe ( HIVTreatmentReviewId, HIVTreatmentReview )) (Maybe MedicationTask)
--   -- SYMPTOM REVIEW
-- | SetSymptomReviewBoolInput (Bool -> SymptomReviewForm -> SymptomReviewForm) Bool
-- | SaveSymptomReview PersonId (Maybe ( HIVSymptomReviewId, HIVSymptomReview ))
--   -- NEXT STEPS
-- | SetActiveNextStepsTask NextStepsTask
-- | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
-- | SaveHealthEducation PersonId (Maybe ( HIVHealthEducationId, HIVHealthEducation )) (Maybe NextStepsTask)
-- | SetFollowUpOption FollowUpOption
-- | SaveFollowUp PersonId (Maybe ( HIVFollowUpId, HIVFollowUp )) (Maybe NextStepsTask)
-- | SetReferToHealthCenter Bool
-- | SetHandReferralForm Bool
-- | SetReasonForNonReferral ReasonForNonReferral
-- | SaveReferral PersonId (Maybe ( HIVReferralId, HIVReferral )) (Maybe NextStepsTask)
