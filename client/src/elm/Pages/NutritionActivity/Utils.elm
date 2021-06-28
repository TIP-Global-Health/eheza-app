module Pages.NutritionActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NutritionEncounterId)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , ContributingFactorsSign(..)
        , FollowUpOption(..)
        , HeightInCm(..)
        , MuacInCm(..)
        , MuacIndication(..)
        , NutritionAssesment(..)
        , NutritionMeasurement
        , NutritionMeasurements
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (contributingFactorsFormWithDefault, followUpFormWithDefault, healthEducationFormWithDefault, sendToHCFormWithDefault)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (taskCompleted)
import RemoteData exposing (RemoteData(..))
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


generateNutritionAssesment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssesment
generateNutritionAssesment currentDate zscores db assembled =
    let
        measurements =
            assembled.measurements

        muacValue =
            Maybe.map (Tuple.second >> .value) measurements.muac

        nutritionValue =
            Maybe.map (Tuple.second >> .value) measurements.nutrition

        weightValue =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> weightValueFunc
                )
                measurements.weight
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssesment currentDate zscores assembled.participant.person muacValue nutritionValue True weightValue db


expectActivity : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
expectActivity currentDate zscores child isChw data db activity =
    case activity of
        -- Show for children that are at least 6 month old.
        Muac ->
            ageInMonths currentDate child
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        NextSteps ->
            if mandatoryActivitiesCompleted currentDate zscores child isChw data db then
                -- Any assesment require sending to HC.
                generateNutritionAssesment currentDate zscores db data
                    |> List.isEmpty
                    |> not

            else
                False

        -- In all other cases, we always view the ativity.
        _ ->
            True


activityCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
activityCompleted currentDate zscores child isChw data db activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        Height ->
            (not <| expectActivity currentDate zscores child isChw data db Height)
                || isJust measurements.height

        Muac ->
            (not <| expectActivity currentDate zscores child isChw data db Muac)
                || isJust measurements.muac

        Nutrition ->
            isJust measurements.nutrition

        Photo ->
            isJust measurements.photo

        Weight ->
            isJust measurements.weight

        NextSteps ->
            (not <| expectActivity currentDate zscores child isChw data db NextSteps)
                || (isJust measurements.sendToHC
                        && isJust measurements.healthEducation
                        && isJust measurements.contributingFactors
                        && isJust measurements.followUp
                   )


mandatoryActivitiesCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> Bool
mandatoryActivitiesCompleted currentDate zscores child isChw data db =
    allMandatoryActivities isChw
        |> List.filter (not << activityCompleted currentDate zscores child isChw data db)
        |> List.isEmpty


{-| List of activities that need to be completed, in order to
decide if to show Next Steps activity, or not.
-}
allMandatoryActivities : Bool -> List NutritionActivity
allMandatoryActivities isChw =
    if isChw then
        [ Muac, Nutrition, Weight ]

    else
        [ Height, Muac, Nutrition, Weight ]


nextStepsTasksCompletedFromTotal : NutritionMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal measurements data task =
    case task of
        NextStepsSendToHC ->
            let
                form =
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
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
                    measurements.healthEducation
                        |> Maybe.map (Tuple.second >> .value)
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
                    measurements.contributingFactors
                        |> Maybe.map (Tuple.second >> .value)
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
            in
            ( taskCompleted form.signs
            , 1
            )

        NextStepFollowUp ->
            let
                form =
                    measurements.followUp
                        |> Maybe.map (Tuple.second >> .value)
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )
