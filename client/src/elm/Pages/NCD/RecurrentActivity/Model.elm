module Pages.NCD.RecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( LaboratoryTask
        , RandomBloodSugarResultForm
        , UrineDipstickResultForm
        , emptyRandomBloodSugarResultForm
        , emptyUrineDipstickResultForm
        )
import Pages.NCD.RecurrentActivity.Types exposing (..)
import Pages.Page exposing (Page)


type Msg
    = NoOp
    | SetActivePage Page
      -- Lab Results msgs
    | SetActiveLabResultsTask LaboratoryTask
    | SetProtein String
    | SetPH String
    | SetGlucose String
    | SetLeukocytes String
    | SetNitrite String
    | SetUrobilinogen String
    | SetHaemoglobin String
    | SetKetone String
    | SetBilirubin String
    | SaveUrineDipstickResult PersonId (Maybe ( NCDUrineDipstickTestId, NCDUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugar String
    | SaveRandomBloodSugarResult PersonId (Maybe ( NCDRandomBloodSugarTestId, NCDRandomBloodSugarTest )) (Maybe LaboratoryTask)
      -- NextStepsMsgs
      -- | SetActiveNextStepsTask NextStepsTask
      -- | SetReferralBoolInput (Bool -> ReferralForm -> ReferralForm) Bool
      -- | SetFacilityNonReferralReason (Maybe ReasonForNonReferral) ReferralFacility ReasonForNonReferral
      -- | SaveSendToHC PersonId (Maybe ( NCDSendToHCId, NCDSendToHC )) (Maybe NextStepsTask)
      -- | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
      -- | SetMedicationDistributionAdministrationNote (Maybe AdministrationNote) MedicationDistributionSign AdministrationNote
      -- | SetRecommendedTreatmentSign (List RecommendedTreatmentSign) RecommendedTreatmentSign
      -- | SaveMedicationDistribution PersonId (Maybe ( NCDMedicationDistributionId, NCDMedicationDistribution )) (Maybe NextStepsTask)
      -- | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
      -- | SaveHealthEducation PersonId (Maybe ( NCDHealthEducationId, NCDHealthEducation )) (Maybe NextStepsTask)
    | CloseLabsResultsEntry PersonId NCDLabsResultsId LabsResultsValue


type alias Model =
    { labResultsData : LabResultsData

    -- , nextStepsData : NextStepsData
    }


emptyModel : Model
emptyModel =
    { labResultsData = emptyLabResultsData

    -- , nextStepsData = emptyNextStepsData
    }


type alias LabResultsData =
    { randomBloodSugarTestForm : RandomBloodSugarResultForm NCDEncounterId
    , urineDipstickTestForm : UrineDipstickResultForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLabResultsData : LabResultsData
emptyLabResultsData =
    { randomBloodSugarTestForm = emptyRandomBloodSugarResultForm
    , urineDipstickTestForm = emptyUrineDipstickResultForm
    , activeTask = Nothing
    }



-- type alias NextStepsData =
--     { referralForm : ReferralForm
--     , medicationDistributionForm : MedicationDistributionForm
--     , healthEducationForm : HealthEducationForm
--     , activeTask : Maybe NextStepsTask
--     }
--
--
-- emptyNextStepsData : NextStepsData
-- emptyNextStepsData =
--     { referralForm = emptyReferralForm
--     , medicationDistributionForm = emptyMedicationDistributionForm
--     , healthEducationForm = emptyHealthEducationForm
--     , activeTask = Nothing
--     }
