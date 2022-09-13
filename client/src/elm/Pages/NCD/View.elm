module Pages.NCD.View exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Model
    exposing
        ( ContentAndTasksForPerformedLaboratoryTestConfig
        , ContentAndTasksLaboratoryTestInitialConfig
        , CorePhysicalExamForm
        , InvokationModule(..)
        , LaboratoryTask(..)
        , OutsideCareStep(..)
        , VitalsForm
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , emptyContentAndTasksForPerformedLaboratoryTestConfig
        , emptyContentAndTasksLaboratoryTestInitialConfig
        , familyPlanningFormWithDefault
        , hivTestFormWithDefault
        , laboratoryTaskIconClass
        , nonRDTFormWithDefault
        , outsideCareFormInputsAndTasks
        , outsideCareFormWithDefault
        , pregnancyTestFormWithDefault
        , randomBloodSugarFormWithDefault
        , urineDipstickFormWithDefault
        , viewHIVTestForm
        , viewNonRDTForm
        , viewPregnancyTestForm
        , viewRandomBloodSugarForm
        , viewUrineDipstickForm
        , vitalsFormWithDefault
        )
import Pages.NCD.Model exposing (..)
import Pages.NCD.Utils
    exposing
        ( medicationDistributionFormWithDefault
        , referralFormWithDefault
        , resolveMedicationDistributionInputsAndTasks
        , resolveReferralInputsAndTasks
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( isTaskCompleted
        , saveButton
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewNumberInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        )
import Translate exposing (Language, TranslationId, translate)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> NCDEncounterPhase
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate phase setRecommendedTreatmentSignMsg setMedicationDistributionBoolInputMsg assembled form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                phase
                assembled
                setRecommendedTreatmentSignMsg
                setMedicationDistributionBoolInputMsg
                form
    in
    div [ class "ui form medication-distribution" ]
        content


viewReferralForm :
    Language
    -> NominalDate
    -> NCDEncounterPhase
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> AssembledData
    -> ReferralForm
    -> Html msg
viewReferralForm language currentDate phase setReferralBoolInputMsg setFacilityNonReferralReasonMsg assembled form =
    let
        ( inputs, _ ) =
            resolveReferralInputsAndTasks language
                currentDate
                phase
                assembled
                setReferralBoolInputMsg
                setFacilityNonReferralReasonMsg
                form
    in
    div [ class "ui form referral" ]
        inputs
