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
import Pages.Utils exposing (viewCheckBoxSelectInputWithRecommendation, viewCustomLabel, viewEncounterActionButton, viewInstructionsLabel)
import Translate exposing (Language, translate)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> PrenatalEncounterPhase
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> (AvoidingGuidanceReason -> msg)
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate phase assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg setRecommendedTreatmentSignMsg avoidingGuidanceReasonMsg form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                phase
                assembled
                setMedicationDistributionBoolInputMsg
                setMedicationDistributionAdministrationNoteMsg
                setRecommendedTreatmentSignMsg
                avoidingGuidanceReasonMsg
                form
    in
    div [ class "ui form medication-distribution" ]
        content


viewPauseEncounterButton : Language -> Bool -> msg -> Html msg
viewPauseEncounterButton language enabled pauseAction =
    viewEncounterActionButton language Translate.PauseEncounter enabled pauseAction


customWarningPopup : Language -> ( Html msg, Html msg, msg ) -> Html msg
customWarningPopup language ( topMessage, bottomMessage, action ) =
    div [ class "ui active modal diagnosis-popup" ]
        [ div [ class "content" ] <|
            [ div [ class "popup-heading-wrapper" ]
                [ img [ src "assets/images/exclamation-red.png" ] []
                , div [ class "popup-heading" ] [ text <| translate language Translate.Warning ++ "!" ]
                ]
            , div [ class "popup-title" ]
                [ topMessage
                , bottomMessage
                ]
            ]
        , div
            [ class "actions" ]
            [ button
                [ class "ui primary fluid button"
                , onClick action
                ]
                [ text <| translate language Translate.Continue ]
            ]
        ]
