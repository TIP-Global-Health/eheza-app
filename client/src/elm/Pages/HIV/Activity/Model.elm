module Pages.HIV.Activity.Model exposing (DiagnosticsData, DiagnosticsForm, HealthEducationForm, MedicationData, MedicationTask(..), Model, Msg(..), NextStepsData, NextStepsTask(..), PrescribedMedicationForm, SymptomReviewData, SymptomReviewForm, emptyDiagnosticsData, emptyDiagnosticsForm, emptyHealthEducationForm, emptyMedicationData, emptyModel, emptyNextStepsData, emptyPrescribedMedicationForm, emptySymptomReviewData, emptySymptomReviewForm)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
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
    , showEndEncounterDialog : Bool
    }


emptyDiagnosticsData : DiagnosticsData
emptyDiagnosticsData =
    { form = emptyDiagnosticsForm
    , showEndEncounterDialog = False
    }


type alias DiagnosticsForm =
    { -- Used in case we don't have info of positive HIV test result
      -- for this patient (HIV test is done at Prenatal and NCD).
      resultPositive : Maybe Bool
    , -- Used in case we have info of positive HIV test result
      -- for this patient (HIV test is done at Prenatal and NCD).
      resultDateCorrect : Maybe Bool
    , positiveResultDate : Maybe Date
    , positiveResultDateDirty : Bool
    , positiveResultDateEstimated : Maybe Bool
    , positiveResultDateEstimatedDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)

    -- Used in case patient reports of not being Diagnosed with HIV.
    , runHIVTest : Maybe Bool
    , runHIVTestDirty : Bool
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    }


emptyDiagnosticsForm : DiagnosticsForm
emptyDiagnosticsForm =
    { resultPositive = Nothing
    , resultDateCorrect = Nothing
    , positiveResultDate = Nothing
    , positiveResultDateDirty = False
    , positiveResultDateEstimated = Nothing
    , positiveResultDateEstimatedDirty = False
    , dateSelectorPopupState = Nothing
    , runHIVTest = Nothing
    , runHIVTestDirty = False
    , testResult = Nothing
    , testResultDirty = False
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
    { medicationsNotChanged : Maybe Bool
    , medications : Maybe (List HIVPrescribedMedication)
    }


emptyPrescribedMedicationForm : PrescribedMedicationForm
emptyPrescribedMedicationForm =
    { medicationsNotChanged = Nothing
    , medications = Nothing
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
    { symptoms : Maybe (List HIVSymptom)
    , symptomsDirty : Bool
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    { symptoms = Nothing
    , symptomsDirty = False
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
    { positiveResult : Maybe Bool
    , saferSexPractices : Maybe Bool
    , encouragedPartnerTesting : Maybe Bool
    , familyPlanningOptions : Maybe Bool
    }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    { positiveResult = Nothing
    , saferSexPractices = Nothing
    , encouragedPartnerTesting = Nothing
    , familyPlanningOptions = Nothing
    }


type NextStepsTask
    = TaskReferral
    | TaskHealthEducation
    | TaskFollowUp


type Msg
    = SetActivePage Page
      -- DIAGNOSTICS
    | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
    | ConfirmPositiveResultDate Date Bool
    | SetPositiveResultDate Date
    | SetHIVTestResult String
    | SetEndEncounterDialogState Bool (Maybe (DiagnosticsForm -> DiagnosticsForm))
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveDiagnostics PersonId IndividualEncounterParticipantId Bool (Maybe ( HIVDiagnosticsId, HIVDiagnostics ))
      -- MEDICATION
    | SetActiveMedicationTask MedicationTask
    | SetPrescribedMedicationsNotChanged Bool
    | SetPrescribedMedication HIVPrescribedMedication
    | SavePrescribedMedication PersonId (Maybe ( HIVMedicationId, HIVMedication )) (Maybe MedicationTask)
    | SetTreatmentReviewBoolInput (Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) Bool
    | SetReasonForNotTaking ReasonForNotTaking
    | SetTotalMissedDoses String
    | SetAdverseEvent AdverseEvent
    | SaveTreatmentReview PersonId (Maybe ( HIVTreatmentReviewId, HIVTreatmentReview )) (Maybe MedicationTask)
      -- SYMPTOM REVIEW
    | SetSymptom HIVSymptom
    | SaveSymptomReview PersonId (Maybe ( HIVSymptomReviewId, HIVSymptomReview ))
      -- NEXT STEPS
    | SetActiveNextStepsTask NextStepsTask
    | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducation PersonId (Maybe ( HIVHealthEducationId, HIVHealthEducation )) (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( HIVFollowUpId, HIVFollowUp )) (Maybe NextStepsTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNonReferral ReasonForNonReferral
    | SaveReferral PersonId (Maybe ( HIVReferralId, HIVReferral )) (Maybe NextStepsTask)
