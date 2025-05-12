module Pages.Nutrition.Activity.Utils exposing (..)

import Backend.Measurement.Model
    exposing
        ( NutritionAssessment
        , NutritionMeasurements
        , ReferralFacility(..)
        , SkippedForm(..)
        )
import Backend.Measurement.Utils exposing (expectNCDAActivity, getMeasurementValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet exposing (EverySet)
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
import Pages.Nutrition.Activity.Model exposing (..)
import Pages.Nutrition.Encounter.Model exposing (AssembledData)
import Pages.Utils exposing (resolveTasksCompletedFromTotal, taskCompleted)
import SyncManager.Model exposing (SiteFeature)
import Translate.Model exposing (Language(..))
import ZScore.Model


generateNutritionAssessment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssessment
generateNutritionAssessment currentDate zscores db assembled =
    let
        measurements =
            assembled.measurements

        muacValue =
            getMeasurementValueFunc measurements.muac

        nutritionValue =
            getMeasurementValueFunc measurements.nutrition
                |> Maybe.map .signs

        weightValue =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> weightValueFunc
                )
                measurements.weight
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssessment currentDate zscores assembled.participant.person muacValue nutritionValue weightValue True db


expectActivity : NominalDate -> ZScore.Model.Model -> EverySet SiteFeature -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
expectActivity currentDate zscores features isChw assembled db activity =
    case activity of
        -- Show for children that are at least 6 months old.
        Muac ->
            ageInMonths currentDate assembled.person
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        -- For nurses only, show if child is bellow age of 24 months.
        NCDA ->
            expectNCDAActivity currentDate features isChw assembled.person

        NextSteps ->
            if mandatoryActivitiesCompleted currentDate zscores features assembled.person isChw assembled db then
                -- Any assesment require sending to HC.
                generateNutritionAssessment currentDate zscores db assembled
                    |> List.isEmpty
                    |> not

            else
                False

        -- In all other cases, we always view the ativity.
        _ ->
            True


activityCompleted : NominalDate -> ZScore.Model.Model -> EverySet SiteFeature -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
activityCompleted currentDate zscores features isChw assembled db activity =
    let
        measurements =
            assembled.measurements
    in
    case activity of
        Height ->
            (not <| expectActivity currentDate zscores features isChw assembled db Height)
                || isJust measurements.height
                || EverySet.member SkippedHeight assembled.encounter.skippedForms

        Muac ->
            (not <| expectActivity currentDate zscores features isChw assembled db Muac)
                || isJust measurements.muac

        Nutrition ->
            isJust measurements.nutrition

        Photo ->
            isJust measurements.photo

        Weight ->
            isJust measurements.weight
                || EverySet.member SkipppedWeight assembled.encounter.skippedForms

        NCDA ->
            isJust measurements.ncda

        NextSteps ->
            (not <| expectActivity currentDate zscores features isChw assembled db NextSteps)
                || (isJust measurements.sendToHC
                        && isJust measurements.healthEducation
                        && isJust measurements.contributingFactors
                        && isJust measurements.followUp
                   )


mandatoryActivitiesCompleted : NominalDate -> ZScore.Model.Model -> EverySet SiteFeature -> Person -> Bool -> AssembledData -> ModelIndexedDb -> Bool
mandatoryActivitiesCompleted currentDate zscores features child isChw assembled db =
    allMandatoryActivities isChw
        |> List.all (activityCompleted currentDate zscores features isChw assembled db)


{-| List of activities that need to be completed, in order to
decide if to show Next Steps activity, or not.
-}
allMandatoryActivities : Bool -> List NutritionActivity
allMandatoryActivities isChw =
    -- Height can be skipped for Burundi and Rwanda CHW.
    -- Weight cab be skipped only for Burundi CHW.
    [ Height, Muac, Nutrition, Weight ]


nextStepsTasksCompletedFromTotal : NominalDate -> NutritionMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate measurements data task =
    let
        ( _, tasks ) =
            case task of
                NextStepsSendToHC ->
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityInputsAndTasks English
                            currentDate
                            FacilityHealthCenter
                            Pages.Nutrition.Activity.Model.SetReferToHealthCenter
                            Pages.Nutrition.Activity.Model.SetReasonForNonReferral
                            Pages.Nutrition.Activity.Model.SetHandReferralForm
                            Nothing

                NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInutsAndTasks English
                            currentDate
                            Pages.Nutrition.Activity.Model.SetProvidedEducationForDiagnosis
                            Pages.Nutrition.Activity.Model.SetReasonForNotProvidingHealthEducation

                NextStepContributingFactors ->
                    getMeasurementValueFunc measurements.contributingFactors
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> contributingFactorsFormInutsAndTasks English
                            currentDate
                            Pages.Nutrition.Activity.Model.SetContributingFactorsSign

                NextStepFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> followUpFormInputsAndTasks English
                            currentDate
                            []
                            Pages.Nutrition.Activity.Model.SetFollowUpOption
    in
    resolveTasksCompletedFromTotal tasks
