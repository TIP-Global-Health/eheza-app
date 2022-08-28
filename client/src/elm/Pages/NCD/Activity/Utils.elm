module Pages.NCD.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NCDEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
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

        Examination ->
            True

        FamilyPlanning ->
            isPersonAFertileWoman currentDate assembled.person

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

        Examination ->
            isJust assembled.measurements.coreExam
                && isJust assembled.measurements.vitals

        FamilyPlanning ->
            (not <| expectActivity currentDate assembled db FamilyPlanning)
                || isJust assembled.measurements.familyPlanning

        -- @todo
        _ ->
            False


dangerSignsFormWithDefault : DangerSignsForm -> Maybe NCDDangerSignsValue -> DangerSignsForm
dangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toDangerSignsValueWithDefault : Maybe NCDDangerSignsValue -> DangerSignsForm -> Maybe NCDDangerSignsValue
toDangerSignsValueWithDefault saved form =
    dangerSignsFormWithDefault form saved
        |> toDangerSignsValue


toDangerSignsValue : DangerSignsForm -> Maybe NCDDangerSignsValue
toDangerSignsValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDDangerSigns) form.signs


symptomReviewFormWithDefault : SymptomReviewForm -> Maybe NCDSymptomReviewValue -> SymptomReviewForm
symptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { group1Symptoms = or form.group1Symptoms (EverySet.toList value.group1Symptoms |> Just)
                , group2Symptoms = or form.group2Symptoms (EverySet.toList value.group2Symptoms |> Just)
                , painSymptoms = or form.painSymptoms (EverySet.toList value.painSymptoms |> Just)
                }
            )


toSymptomReviewValueWithDefault : Maybe NCDSymptomReviewValue -> SymptomReviewForm -> Maybe NCDSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


toSymptomReviewValue : SymptomReviewForm -> Maybe NCDSymptomReviewValue
toSymptomReviewValue form =
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
