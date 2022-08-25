module Pages.NCD.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NCDEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, taskCompleted)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
expectActivity currentDate assembled db activity =
    case activity of
        DangerSigns ->
            True

        SymptomReview ->
            True

        -- @todo
        _ ->
            True


activityCompleted : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
activityCompleted currentDate assembled db activity =
    case activity of
        DangerSigns ->
            isJust assembled.measurements.dangerSigns

        SymptomReview ->
            isJust assembled.measurements.symptomReview

        -- @todo
        _ ->
            False


ncdDangerSignsFormWithDefault : NCDDangerSignsForm -> Maybe NCDDangerSignsValue -> NCDDangerSignsForm
ncdDangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNCDDangerSignsValueWithDefault : Maybe NCDDangerSignsValue -> NCDDangerSignsForm -> Maybe NCDDangerSignsValue
toNCDDangerSignsValueWithDefault saved form =
    ncdDangerSignsFormWithDefault form saved
        |> toNCDDangerSignsValue


toNCDDangerSignsValue : NCDDangerSignsForm -> Maybe NCDDangerSignsValue
toNCDDangerSignsValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDDangerSigns) form.signs


ncdSymptomReviewFormWithDefault : NCDSymptomReviewForm -> Maybe NCDSymptomReviewValue -> NCDSymptomReviewForm
ncdSymptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { group1Symptoms = or form.group1Symptoms (EverySet.toList value.group1Symptoms |> Just)
                , group2Symptoms = or form.group2Symptoms (EverySet.toList value.group2Symptoms |> Just)
                , painSymptoms = or form.painSymptoms (EverySet.toList value.painSymptoms |> Just)
                }
            )


toNCDSymptomReviewValueWithDefault : Maybe NCDSymptomReviewValue -> NCDSymptomReviewForm -> Maybe NCDSymptomReviewValue
toNCDSymptomReviewValueWithDefault saved form =
    ncdSymptomReviewFormWithDefault form saved
        |> toNCDSymptomReviewValue


toNCDSymptomReviewValue : NCDSymptomReviewForm -> Maybe NCDSymptomReviewValue
toNCDSymptomReviewValue form =
    let
        group1Symptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDGroup1Symptoms) form.group1Symptoms

        group2Symptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDGroup2Symptoms) form.group2Symptoms

        painSymptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDPainSymptoms) form.painSymptoms
    in
    Maybe.map NCDSymptomReviewValue group1Symptoms
        |> andMap group2Symptoms
        |> andMap painSymptoms
