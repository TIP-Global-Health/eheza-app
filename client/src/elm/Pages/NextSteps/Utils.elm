module Pages.NextSteps.Utils exposing (nextStepsTasksCompletedFromTotal)

import Backend.Measurement.Model
    exposing
        ( ChildMeasurements
        , MeasurementData
        , ReferralFacility(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, mapMeasurementData)
import Measurement.Model exposing (NextStepsTask(..))
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
import Pages.NextSteps.Model exposing (Model)
import Pages.Utils exposing (resolveTasksCompletedFromTotal)
import Translate.Model exposing (Language(..))


nextStepsTasksCompletedFromTotal : MeasurementData ChildMeasurements -> Model -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal measurements data task =
    let
        ( _, tasks ) =
            case task of
                NextStepsSendToHC ->
                    mapMeasurementData .sendToHC measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityInputsAndTasks English
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
                            Pages.NextSteps.Model.SetProvidedEducationForDiagnosis
                            Pages.NextSteps.Model.SetReasonForNotProvidingHealthEducation

                NextStepContributingFactors ->
                    mapMeasurementData .contributingFactors measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> contributingFactorsFormInutsAndTasks English
                            Pages.NextSteps.Model.SetContributingFactorsSign

                NextStepFollowUp ->
                    mapMeasurementData .followUp measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> followUpFormInputsAndTasks English
                            []
                            Pages.NextSteps.Model.SetFollowUpOption
    in
    resolveTasksCompletedFromTotal tasks
