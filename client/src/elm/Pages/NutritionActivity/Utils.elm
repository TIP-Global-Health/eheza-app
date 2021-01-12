module Pages.NutritionActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), HeightInCm(..), MuacInCm(..), NutritionMeasurement, NutritionMeasurements, WeightInKg(..))
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Maybe.Extra exposing (isJust, or, unwrap)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, valueConsideringIsDirtyField)


expectActivity : NominalDate -> Person -> Bool -> NutritionMeasurements -> NutritionActivity -> Bool
expectActivity currentDate child isChw measurements activity =
    case activity of
        -- Do not show for community health workers.
        Height ->
            not isChw

        -- Show for children that are at least 6 month old.
        Muac ->
            child.birthDate
                |> Maybe.map
                    (\birthDate -> diffMonths birthDate currentDate > 5)
                |> Maybe.withDefault False

        SendToHC ->
            mandatoryActivitiesCompleted measurements

        _ ->
            True


activityCompleted : NominalDate -> Person -> Bool -> NutritionMeasurements -> NutritionActivity -> Bool
activityCompleted currentDate child isChw measurements activity =
    case activity of
        Height ->
            isJust measurements.height

        Muac ->
            isJust measurements.muac

        Nutrition ->
            isJust measurements.nutrition

        Photo ->
            isJust measurements.photo

        Weight ->
            isJust measurements.weight

        SendToHC ->
            -- @todo
            {--isJust measurements.sendToHC  || --}
            not <| expectActivity currentDate child isChw measurements SendToHC


mandatoryActivitiesCompleted : NutritionMeasurements -> Bool
mandatoryActivitiesCompleted measurements =
    -- @todo
    True


resolvePreviousIndividualValue : AssembledData -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a )) -> (a -> b) -> Maybe ( NominalDate, b )
resolvePreviousIndividualValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( date, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map
                        (\measurement ->
                            ( date, Tuple.second measurement |> .value |> valueFunc )
                        )
            )
        |> List.reverse
        |> List.head


fromMuacValue : Maybe MuacInCm -> MuacForm
fromMuacValue saved =
    { muac = Maybe.map (\(MuacInCm cm) -> cm) saved
    , muacDirty = False
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = valueConsideringIsDirtyField form.muacDirty form.muac (value |> (\(MuacInCm cm) -> cm))
                , muacDirty = form.muacDirty
                }
            )


toMuacValueWithDefault : Maybe MuacInCm -> MuacForm -> Maybe MuacInCm
toMuacValueWithDefault saved form =
    muacFormWithDefault form saved
        |> toMuacValue


toMuacValue : MuacForm -> Maybe MuacInCm
toMuacValue form =
    Maybe.map MuacInCm form.muac


fromHeightValue : Maybe HeightInCm -> HeightForm
fromHeightValue saved =
    { height = Maybe.map (\(HeightInCm cm) -> cm) saved
    , heightDirty = False
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (value |> (\(HeightInCm cm) -> cm))
                , heightDirty = form.heightDirty
                }
            )


toHeightValueWithDefault : Maybe HeightInCm -> HeightForm -> Maybe HeightInCm
toHeightValueWithDefault saved form =
    heightFormWithDefault form saved
        |> toHeightValue


toHeightValue : HeightForm -> Maybe HeightInCm
toHeightValue form =
    Maybe.map HeightInCm form.height


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


fromWeightValue : Maybe WeightInKg -> WeightForm
fromWeightValue saved =
    { weight = Maybe.map (\(WeightInKg cm) -> cm) saved
    , weightDirty = False
    }


weightFormWithDefault : WeightForm -> Maybe WeightInKg -> WeightForm
weightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { weight = valueConsideringIsDirtyField form.weightDirty form.weight (value |> (\(WeightInKg cm) -> cm))
                , weightDirty = form.weightDirty
                }
            )


toWeightValueWithDefault : Maybe WeightInKg -> WeightForm -> Maybe WeightInKg
toWeightValueWithDefault saved form =
    weightFormWithDefault form saved
        |> toWeightValue


toWeightValue : WeightForm -> Maybe WeightInKg
toWeightValue form =
    Maybe.map WeightInKg form.weight
