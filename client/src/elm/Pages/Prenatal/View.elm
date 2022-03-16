module Pages.Prenatal.View exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils exposing (viewCheckBoxSelectInput, viewCustomLabel, viewInstructionsLabel)
import Translate exposing (Language, translate)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> List MedicationDistributionSign
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg allowedMedications form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                assembled
                setMedicationDistributionBoolInputMsg
                setMedicationDistributionAdministrationNoteMsg
                allowedMedications
                form
    in
    div [ class "ui form medication-distribution" ] <|
        h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
            :: content


viewRecommendedTreatmentForHypertension :
    Language
    -> NominalDate
    -> (RecommendedTreatmentSign -> msg)
    -> RecommendedTreatmentForm
    -> List (Html msg)
viewRecommendedTreatmentForHypertension language currentDate setRecommendedTreatmentSignMsg form =
    let
        -- Since we may have values set for other diagnoses, we need
        -- to filter them out, to be able to determine current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                    >> List.head
                )
                form.signs
    in
    [ viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
    , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
    , div [ class "instructions" ]
        [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.HypertensionRecommendedTreatmentHelper ++ ":") ]
    , viewCheckBoxSelectInput language
        recommendedTreatmentSignsForHypertension
        []
        currentValue
        setRecommendedTreatmentSignMsg
        Translate.RecommendedTreatmentSignLabel
    ]
