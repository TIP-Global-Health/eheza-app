module Pages.NextSteps.Utils exposing (..)

import Backend.Measurement.Model
    exposing
        ( ChildMeasurements
        , MeasurementData
        , ReferralFacility(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, mapMeasurementData)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (..)
import Measurement.Utils
    exposing
        ( contributingFactorsFormWithDefault
        , healthEducationFormWithDefault
        , nutritionFollowUpFormWithDefault
        , sendToHCFormWithDefault
        )
import Measurement.View
    exposing
        ( contributingFactorsFormInutsAndTasks
        , followUpFormInputsAndTasks
        , healthEducationFormInutsAndTasks
        , sendToFacilityInputsAndTasks
        )
import Pages.NextSteps.Model exposing (..)
import Pages.Utils exposing (resolveTasksCompletedFromTotal, taskCompleted)
import Translate.Model exposing (Language(..))


nextStepsTasksCompletedFromTotal : NominalDate -> MeasurementData ChildMeasurements -> Model -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate measurements data task =
    let
        ( _, tasks ) =
            case task of
                NextStepsSendToHC ->
                    mapMeasurementData .sendToHC measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityInputsAndTasks English
                            currentDate
                            FacilityHealthCenter
                            Pages.NextSteps.Model.SetReferToHealthCenter
                            Pages.NextSteps.Model.SetReasonForNonReferral
                            Pages.NextSteps.Model.SetHandReferralForm
                            Nothing

                NextStepsHealthEducation ->
                    mapMeasurementData .healthEducation measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInutsAndTasks English
                            currentDate
                            Pages.NextSteps.Model.SetProvidedEducationForDiagnosis
                            Pages.NextSteps.Model.SetReasonForNotProvidingHealthEducation

                NextStepContributingFactors ->
                    mapMeasurementData .contributingFactors measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> contributingFactorsFormInutsAndTasks English
                            currentDate
                            Pages.NextSteps.Model.SetContributingFactorsSign

                NextStepFollowUp ->
                    mapMeasurementData .followUp measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> followUpFormInputsAndTasks English
                            currentDate
                            []
                            Pages.NextSteps.Model.SetFollowUpOption
    in
    resolveTasksCompletedFromTotal tasks
