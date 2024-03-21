module Pages.NextSteps.Utils exposing (..)

import Backend.Measurement.Model
    exposing
        ( ChildMeasurements
        , MeasurementData
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, mapMeasurementData)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (contributingFactorsFormWithDefault, healthEducationFormWithDefault, nutritionFollowUpFormWithDefault, sendToHCFormWithDefault)
import Pages.NextSteps.Model exposing (..)
import Pages.Utils exposing (taskCompleted)


nextStepsTasksCompletedFromTotal : MeasurementData ChildMeasurements -> Model -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal measurements data task =
    case task of
        NextStepsSendToHC ->
            let
                form =
                    mapMeasurementData .sendToHC measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm

                ( reasonForNotSentActive, reasonForNotSentCompleted ) =
                    form.referToHealthCenter
                        |> Maybe.map
                            (\sentToHC ->
                                if not sentToHC then
                                    if isJust form.reasonForNotSendingToHC then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( reasonForNotSentActive + taskCompleted form.handReferralForm
            , reasonForNotSentCompleted + 1
            )

        NextStepsHealthEducation ->
            let
                form =
                    mapMeasurementData .healthEducation measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( reasonForProvidingEducationActive, reasonForProvidingEducationCompleted ) =
                    form.educationForDiagnosis
                        |> Maybe.map
                            (\providedHealthEducation ->
                                if not providedHealthEducation then
                                    if isJust form.reasonForNotProvidingHealthEducation then
                                        ( 1, 1 )

                                    else
                                        ( 0, 1 )

                                else
                                    ( 0, 0 )
                            )
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( reasonForProvidingEducationActive + taskCompleted form.educationForDiagnosis
            , reasonForProvidingEducationCompleted + 1
            )

        NextStepContributingFactors ->
            let
                form =
                    mapMeasurementData .contributingFactors measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
            in
            ( taskCompleted form.signs
            , 1
            )

        NextStepFollowUp ->
            let
                form =
                    mapMeasurementData .followUp measurements
                        |> .current
                        |> getMeasurementValueFunc
                        |> nutritionFollowUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )
