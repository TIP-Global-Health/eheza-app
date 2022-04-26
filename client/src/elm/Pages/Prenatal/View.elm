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
import Pages.Utils exposing (viewCheckBoxSelectInputWithRecommendation, viewCustomLabel, viewInstructionsLabel)
import Translate exposing (Language, translate)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> PrenatalEncounterPhase
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate phase assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg setRecommendedTreatmentSignMsg form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                phase
                assembled
                setMedicationDistributionBoolInputMsg
                setMedicationDistributionAdministrationNoteMsg
                setRecommendedTreatmentSignMsg
                form
    in
    div [ class "ui form medication-distribution" ] <|
        h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
            :: content



-- @todo:
-- viewRecommendedTreatmentForHypertension :
--     Language
--     -> NominalDate
--     -> (RecommendedTreatmentSign -> msg)
--     -> AssembledData
--     -> RecommendedTreatmentForm
--     -> List (Html msg)
-- viewRecommendedTreatmentForHypertension language currentDate setRecommendedTreatmentSignMsg assembled form =
--     let
--         -- Since we may have values set for other diagnoses, we need
--         -- to filter them out, to be able to determine current value.
--         currentValue =
--             Maybe.andThen
--                 (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
--                     >> List.head
--                 )
--                 form.signs
--
--         recommendedSign =
--             recommendTreatmentForHypertension assembled
--     in
--     [ viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
--     , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
--     , div [ class "instructions" ]
--         [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.HypertensionRecommendedTreatmentHelper ++ ":") ]
--     , viewCheckBoxSelectInputWithRecommendation language
--         recommendedTreatmentSignsForHypertension
--         []
--         recommendedSign
--         currentValue
--         setRecommendedTreatmentSignMsg
--         Translate.RecommendedTreatmentSignLabel
--     ]


viewPauseEncounterButton : Language -> Bool -> msg -> Html msg
viewPauseEncounterButton language enabled pauseAction =
    let
        attributes =
            if enabled then
                [ class "ui fluid primary button"
                , onClick pauseAction
                ]

            else
                [ class "ui fluid primary button disabled" ]
    in
    div [ class "actions" ]
        [ button attributes
            [ text <| translate language Translate.PauseEncounter ]
        ]
