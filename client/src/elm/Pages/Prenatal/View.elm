module Pages.Prenatal.View exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils exposing (viewCheckBoxSelectCustomInput, viewCheckBoxSelectInput, viewCustomLabel, viewInstructionsLabel)
import Translate exposing (Language, TranslationId, translate)


viewMedicationDistributionForm :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> Html msg
viewMedicationDistributionForm language currentDate assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        ( content, _, _ ) =
            resolveMedicationDistributionInputsAndTasks language currentDate assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form
    in
    div [ class "ui form medication-distribution" ] <|
        h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
            :: content


viewRecommendedTreatmentForm :
    Language
    -> NominalDate
    -> AssembledData
    -> (RecommendedTreatmentSign -> msg)
    -> RecommendedTreatmentForm
    -> Html msg
viewRecommendedTreatmentForm language currentDate assembled setRecommendedTreatmentSignMsg form =
    let
        content =
            if List.any (\diagnosis -> diagnosed diagnosis assembled) [ DiagnosisMalaria, DiagnosisMalariaWithAnemia, DiagnosisMalariaWithSevereAnemia ] then
                viewRecommendedTreatmentForMalaria language currentDate assembled setRecommendedTreatmentSignMsg form

            else if List.any (\diagnosis -> diagnosed diagnosis assembled) [ DiagnosisSyphilis, DiagnosisSyphilisWithComplications ] then
                viewRecommendedTreatmentForSyphilis language currentDate assembled setRecommendedTreatmentSignMsg form

            else
                []
    in
    div [ class "ui form recommended-treatment" ]
        content


viewRecommendedTreatmentForMalaria :
    Language
    -> NominalDate
    -> AssembledData
    -> (RecommendedTreatmentSign -> msg)
    -> RecommendedTreatmentForm
    -> List (Html msg)
viewRecommendedTreatmentForMalaria language currentDate assembled setRecommendedTreatmentSignMsg form =
    let
        egaInWeeks =
            Maybe.map
                (calculateEGAWeeks currentDate)
                assembled.globalLmpDate

        medicationTreatment =
            Maybe.map
                (\egaWeeks ->
                    if egaWeeks <= 14 then
                        TreatmentQuinineSulphate

                    else
                        TreatmentCoartem
                )
                egaInWeeks
                |> Maybe.withDefault TreatmentQuinineSulphate
    in
    [ viewCustomLabel language Translate.MalariaRecommendedTreatmentHeader "." "instructions"
    , h2 []
        [ text <| translate language Translate.ActionsToTake ++ ":" ]
    , div [ class "instructions" ]
        [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.MalariaRecommendedTreatmentHelper ++ ":") ]
    , viewCheckBoxSelectInput language
        [ medicationTreatment
        , TreatmentWrittenProtocols
        , TreatementReferToHospital
        ]
        []
        (Maybe.andThen List.head form.signs)
        setRecommendedTreatmentSignMsg
        Translate.RecommendedTreatmentSignLabel
    ]


viewRecommendedTreatmentForSyphilis :
    Language
    -> NominalDate
    -> AssembledData
    -> (RecommendedTreatmentSign -> msg)
    -> RecommendedTreatmentForm
    -> List (Html msg)
viewRecommendedTreatmentForSyphilis language currentDate assembled setRecommendedTreatmentSignMsg form =
    let
        warning =
            Maybe.map
                (\signs ->
                    if
                        List.any (\sign -> List.member sign signs)
                            [ TreatementErythromycin, TreatementAzithromycin ]
                    then
                        div [ class "warning" ]
                            [ img [ src "assets/images/exclamation-red.png" ] []
                            , text <| translate language Translate.SyphilisRecommendedTreatmentWarning
                            ]

                    else
                        emptyNode
                )
                form.signs
                |> Maybe.withDefault emptyNode
    in
    [ viewCustomLabel language Translate.SyphilisRecommendedTreatmentHeader "." "instructions"
    , h2 []
        [ text <| translate language Translate.ActionsToTake ++ ":" ]
    , div [ class "instructions" ]
        [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.SyphilisRecommendedTreatmentHelper ++ ".")
        , p [ class "instructions-warning" ] [ text <| translate language Translate.SyphilisRecommendedTreatmentInstructions ++ "." ]
        ]
    , viewCheckBoxSelectCustomInput language
        [ TreatementPenecilin1
        , TreatementPenecilin3
        , TreatementErythromycin
        , TreatementAzithromycin
        , TreatementCeftriaxon
        ]
        []
        (Maybe.andThen List.head form.signs)
        setRecommendedTreatmentSignMsg
        (viewTreatmentOptionForSyphilis language)
    , warning
    ]


viewTreatmentOptionForSyphilis : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionForSyphilis language sign =
    label []
        [ span [ class "treatment" ] [ text <| (translate language <| Translate.RecommendedTreatmentSignLabel sign) ++ ":" ]
        , span [ class "dosage" ] [ text <| translate language <| Translate.RecommendedTreatmentSignDosage sign ]
        ]
