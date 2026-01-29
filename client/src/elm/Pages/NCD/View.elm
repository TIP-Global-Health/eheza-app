module Pages.NCD.View exposing (viewMedicationDistributionForm, viewReferralForm)

import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.NCD.Model exposing (..)
import Pages.NCD.Utils
    exposing
        ( resolveMedicationDistributionInputsAndTasks
        , resolveReferralInputsAndTasks
        )
import Translate exposing (Language)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> NCDEncounterPhase
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate phase setRecommendedTreatmentSignSingleMsg setRecommendedTreatmentSignMultipleMsg setMedicationDistributionBoolInputMsg assembled form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                phase
                assembled
                setRecommendedTreatmentSignSingleMsg
                setRecommendedTreatmentSignMultipleMsg
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
