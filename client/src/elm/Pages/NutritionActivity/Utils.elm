module Pages.NutritionActivity.Utils exposing (fromHeightValue, fromMuacValue, fromNutritionValue, fromWeightValue, heightFormWithDefault, muacFormWithDefault, nutritionFormWithDefault, resolvePreviousIndividualValue, toHeightValue, toHeightValueWithDefault, toMuacValue, toMuacValueWithDefault, toNutritionValue, toNutritionValueWithDefault, toWeightValue, toWeightValueWithDefault, weightFormWithDefault)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), HeightInCm(..), MuacInCm(..), NutritionMeasurement, NutritionMeasurements, WeightInKg(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (or, unwrap)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty)


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
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = or form.muac (value |> (\(MuacInCm cm) -> cm) |> Just)
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
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = or form.height (value |> (\(HeightInCm cm) -> cm) |> Just)
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
    }


weightFormWithDefault : WeightForm -> Maybe WeightInKg -> WeightForm
weightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { weight = or form.weight (value |> (\(WeightInKg cm) -> cm) |> Just)
                }
            )


toWeightValueWithDefault : Maybe WeightInKg -> WeightForm -> Maybe WeightInKg
toWeightValueWithDefault saved form =
    weightFormWithDefault form saved
        |> toWeightValue


toWeightValue : WeightForm -> Maybe WeightInKg
toWeightValue form =
    Maybe.map WeightInKg form.weight
