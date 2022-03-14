module Pages.Prenatal.View exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Prenatal.Model exposing (AssembledData, MedicationDistributionForm)
import Pages.Prenatal.Utils exposing (resolveMedicationDistributionInputsAndTasks)
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
