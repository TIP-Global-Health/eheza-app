module Pages.NCD.RecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( CreatinineResultForm
        , LaboratoryTask
        , LiverFunctionResultForm
        , RandomBloodSugarResultForm
        , UrineDipstickResultForm
        , emptyCreatinineResultForm
        , emptyLiverFunctionResultForm
        , emptyRandomBloodSugarResultForm
        , emptyUrineDipstickResultForm
        )
import Pages.NCD.Model exposing (..)
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
    | SetCreatinineResult String
    | SetUreaResult String
    | SetNitorogenResult String
    | SaveCreatinineResult PersonId (Maybe ( NCDCreatinineTestId, NCDCreatinineTest )) (Maybe LaboratoryTask)
    | SetAltResult String
    | SetAstResult String
    | SaveLiverFunctionResult PersonId (Maybe ( NCDLiverFunctionTestId, NCDLiverFunctionTest )) (Maybe LaboratoryTask)
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetRecommendedTreatmentSignSingle (List RecommendedTreatmentSign) RecommendedTreatmentSign
    | SetRecommendedTreatmentSignMultiple (List RecommendedTreatmentSign) RecommendedTreatmentSign RecommendedTreatmentSign
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SaveMedicationDistribution PersonId (Maybe ( NCDMedicationDistributionId, NCDMedicationDistribution )) (Maybe NextStepsTask)
    | SetReferralBoolInput (Bool -> ReferralForm -> ReferralForm) Bool
    | SetFacilityNonReferralReason (Maybe ReasonForNonReferral) ReferralFacility ReasonForNonReferral
    | SaveReferral PersonId (Maybe ( NCDReferralId, NCDReferral )) (Maybe NextStepsTask)
    | CloseLabsResultsEntry PersonId NCDLabsResultsId LabsResultsValue


type alias Model =
    { labResultsData : LabResultsData
    , nextStepsData : NextStepsData
    }


emptyModel : Model
emptyModel =
    { labResultsData = emptyLabResultsData
    , nextStepsData = emptyNextStepsData
    }


type alias LabResultsData =
    { randomBloodSugarTestForm : RandomBloodSugarResultForm NCDEncounterId
    , urineDipstickTestForm : UrineDipstickResultForm
    , creatinineTestForm : CreatinineResultForm
    , liverFunctionTestForm : LiverFunctionResultForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLabResultsData : LabResultsData
emptyLabResultsData =
    { randomBloodSugarTestForm = emptyRandomBloodSugarResultForm
    , urineDipstickTestForm = emptyUrineDipstickResultForm
    , creatinineTestForm = emptyCreatinineResultForm
    , liverFunctionTestForm = emptyLiverFunctionResultForm
    , activeTask = Nothing
    }


type alias NextStepsData =
    { referralForm : ReferralForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { referralForm = emptyReferralForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , activeTask = Nothing
    }
