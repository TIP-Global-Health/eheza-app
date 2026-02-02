module Pages.NCD.View exposing (viewMedicationDistributionForm, viewReferralForm)

import Backend.Measurement.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.NCD.Model exposing (AssembledData, MedicationDistributionForm, NCDEncounterPhase, ReferralForm)
import Pages.NCD.Utils
    exposing
        ( resolveMedicationDistributionInputsAndTasks
        , resolveReferralInputsAndTasks
        )
import Translate exposing (Language)


viewMedicationDistributionForm :
    Language
    -> NCDEncounterPhase
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language phase setRecommendedTreatmentSignSingleMsg setRecommendedTreatmentSignMultipleMsg setMedicationDistributionBoolInputMsg assembled form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
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
    -> NCDEncounterPhase
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> AssembledData
    -> ReferralForm
    -> Html msg
viewReferralForm language phase setReferralBoolInputMsg setFacilityNonReferralReasonMsg assembled form =
    let
        ( inputs, _ ) =
            resolveReferralInputsAndTasks language
                phase
                assembled
                setReferralBoolInputMsg
                setFacilityNonReferralReasonMsg
                form
    in
    div [ class "ui form referral" ]
        inputs
