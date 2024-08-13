module Pages.Prenatal.View exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( customPopup
        , taskCompleted
        , viewBoolInput
        , viewEncounterActionButton
        , viewQuestionLabel
        , viewTasksCount
        )
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
viewMedicationDistributionForm language currentDate phase assembled setBoolInputMsg setAdministrationNoteMsg setRecommendedTreatmentSignMsg avoidingGuidanceReasonMsg form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language
                currentDate
                phase
                assembled
                setBoolInputMsg
                setAdministrationNoteMsg
                setRecommendedTreatmentSignMsg
                avoidingGuidanceReasonMsg
                form
    in
    div [ class "ui form medication-distribution" ]
        content


viewMalariaPreventionContent :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> MalariaPreventionForm -> MalariaPreventionForm) -> Bool -> msg)
    -> (PersonId -> Maybe ( MalariaPreventionId, MalariaPrevention ) -> msg)
    -> MalariaPreventionData
    -> List (Html msg)
viewMalariaPreventionContent language currentDate assembled setBoolInputMsg saveMsg data =
    let
        form =
            assembled.measurements.malariaPrevention
                |> getMeasurementValueFunc
                |> malariaPreventionFormWithDefault data.form

        tasksCompleted =
            taskCompleted form.receivedMosquitoNet

        totalTasks =
            1

        receivedMosquitoNetUpdateFunc value form_ =
            { form_ | receivedMosquitoNet = Just value }
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form malaria-prevention" ]
                [ viewQuestionLabel language Translate.ReceivedMosquitoNet
                , viewBoolInput
                    language
                    form.receivedMosquitoNet
                    (setBoolInputMsg receivedMosquitoNetUpdateFunc)
                    "mosquito-net"
                    Nothing
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| saveMsg assembled.participant.person assembled.measurements.malariaPrevention
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPauseEncounterButton : Language -> String -> Bool -> msg -> Html msg
viewPauseEncounterButton language buttonColor enabled pauseAction =
    viewEncounterActionButton language Translate.PauseEncounter buttonColor enabled pauseAction


customWarningPopup : Language -> ( Html msg, Html msg, msg ) -> Html msg
customWarningPopup language =
    customPopup language True Translate.Continue "warning-popup"
